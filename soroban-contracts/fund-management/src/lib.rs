#![no_std]
use soroban_sdk::{contract, contractimpl, contracttype, Address, Env, String};

#[contracttype]
#[derive(Clone)]
pub struct EmergencyUnlockRequest {
    pub user: Address,
    pub amount: i128,
    pub reason: String,
    pub proof_url: Option<String>,
    pub approved_votes: u32,
    pub total_votes: u32,
    pub is_approved: bool,
}

#[contracttype]
pub enum DataKey {
    Admin,
    NextRequestId,
    Requests(u32),
    MemberVotes(u32, Address),
}

#[contract]
pub struct FundManagementContract;

#[contractimpl]
impl FundManagementContract {
    pub fn initialize(env: Env, admin: Address) {
        if env.storage().instance().has(&DataKey::Admin) {
            panic!("Already initialized");
        }
        env.storage().instance().set(&DataKey::Admin, &admin);
        env.storage().instance().set(&DataKey::NextRequestId, &1u32);
    }

    pub fn request_emergency_unlock(
        env: Env,
        user: Address,
        reason: String,
        proof_url: Option<String>,
    ) -> u32 {
        user.require_auth();

        let request_id: u32 = env.storage().instance().get(&DataKey::NextRequestId).unwrap_or(1);
        
        let request = EmergencyUnlockRequest {
            user,
            amount: 0, // In full implementation this would hook into lock balances
            reason,
            proof_url,
            approved_votes: 0,
            total_votes: 0,
            is_approved: false,
        };

        env.storage().persistent().set(&DataKey::Requests(request_id), &request);
        env.storage().instance().set(&DataKey::NextRequestId, &(request_id + 1));

        request_id
    }

    pub fn approve_emergency_unlock(env: Env, admin: Address, request_id: u32, approve: bool) {
        admin.require_auth();
        let global_admin: Address = env.storage().instance().get(&DataKey::Admin).expect("Not initialized");
        assert!(admin == global_admin, "Not authorized");

        let mut request: EmergencyUnlockRequest = env.storage().persistent().get(&DataKey::Requests(request_id)).expect("Not found");
        assert!(!request.is_approved, "Already approved");

        let has_voted: bool = env.storage().persistent().get(&DataKey::MemberVotes(request_id, admin.clone())).unwrap_or(false);
        assert!(!has_voted, "Vote exists");

        request.total_votes += 1;
        if approve {
            request.approved_votes += 1;
        }

        // For simplicity, 1 vote from admin approves it.
        // In reality, this might need 3 out of 5 multisig.
        if request.approved_votes > 0 {
            request.is_approved = true;
            // Transfer logic would be called here.
        }

        env.storage().persistent().set(&DataKey::MemberVotes(request_id, admin), &true);
        env.storage().persistent().set(&DataKey::Requests(request_id), &request);
    }
}
