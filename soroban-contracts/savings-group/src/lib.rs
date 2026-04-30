#![no_std]
use soroban_sdk::{contract, contractimpl, contracttype, Address, Env, String, token};

#[contracttype]
#[derive(Clone)]
pub struct Proposal {
    pub proposer: Address,
    pub amount: i128,
    pub description: String,
    pub votes_for: u32,
    pub votes_against: u32,
    pub expiration_timestamp: u64,
}

#[contracttype]
#[derive(Clone)]
pub struct SavingsGroup {
    pub name: String,
    pub token: Address,
    pub total_funds: i128,
    pub member_count: u32,
    pub active_proposal: Option<Proposal>,
}

#[contracttype]
#[derive(Clone)]
pub struct MemberInfo {
    pub joined_at: u64,
    pub contribution: i128,
}

#[contracttype]
pub enum DataKey {
    GroupNonce,
    Groups(u32),
    GroupMembers(u32, Address),
    MemberVotes(u32, Address),
}

#[contract]
pub struct SavingsGroupContract;

#[contractimpl]
impl SavingsGroupContract {
    pub fn create_group(env: Env, name: String, token: Address, creator: Address) -> u32 {
        creator.require_auth();

        let nonce: u32 = env.storage().instance().get(&DataKey::GroupNonce).unwrap_or(0);
        let new_group_id = nonce + 1;

        let group = SavingsGroup {
            name,
            token,
            total_funds: 0,
            member_count: 1,
            active_proposal: None,
        };

        let member_info = MemberInfo {
            joined_at: env.ledger().timestamp(),
            contribution: 0,
        };

        env.storage().persistent().set(&DataKey::Groups(new_group_id), &group);
        env.storage().persistent().set(&DataKey::GroupMembers(new_group_id, creator), &member_info);
        env.storage().instance().set(&DataKey::GroupNonce, &new_group_id);

        new_group_id
    }

    pub fn join_group(env: Env, group_id: u32, member: Address) {
        member.require_auth();
        let mut group: SavingsGroup = env.storage().persistent().get(&DataKey::Groups(group_id)).expect("Group not found");
        assert!(!env.storage().persistent().has(&DataKey::GroupMembers(group_id, member.clone())), "Already a member");

        let member_info = MemberInfo {
            joined_at: env.ledger().timestamp(),
            contribution: 0,
        };

        group.member_count += 1;

        env.storage().persistent().set(&DataKey::GroupMembers(group_id, member), &member_info);
        env.storage().persistent().set(&DataKey::Groups(group_id), &group);
    }

    pub fn contribute(env: Env, group_id: u32, member: Address, amount: i128) {
        member.require_auth();
        assert!(amount > 0, "Invalid amount");

        let mut group: SavingsGroup = env.storage().persistent().get(&DataKey::Groups(group_id)).expect("Group not found");
        let mut member_info: MemberInfo = env.storage().persistent().get(&DataKey::GroupMembers(group_id, member.clone())).expect("Not a member");

        let client = token::Client::new(&env, &group.token);
        client.transfer(&member, &env.current_contract_address(), &amount);

        group.total_funds += amount;
        member_info.contribution += amount;

        env.storage().persistent().set(&DataKey::Groups(group_id), &group);
        env.storage().persistent().set(&DataKey::GroupMembers(group_id, member), &member_info);
    }

    pub fn create_proposal(env: Env, group_id: u32, proposer: Address, amount: i128, description: String) {
        proposer.require_auth();
        let mut group: SavingsGroup = env.storage().persistent().get(&DataKey::Groups(group_id)).expect("Group not found");
        assert!(env.storage().persistent().has(&DataKey::GroupMembers(group_id, proposer.clone())), "Not a member");
        assert!(group.active_proposal.is_none(), "Vote exists");
        assert!(amount <= group.total_funds, "Insufficient funds");

        let proposal = Proposal {
            proposer,
            amount,
            description,
            votes_for: 0,
            votes_against: 0,
            // 24 hours = 86400 seconds
            expiration_timestamp: env.ledger().timestamp() + 86400,
        };

        group.active_proposal = Some(proposal);
        env.storage().persistent().set(&DataKey::Groups(group_id), &group);
    }

    pub fn vote_on_proposal(env: Env, group_id: u32, member: Address, vote_for: bool) {
        member.require_auth();
        let mut group: SavingsGroup = env.storage().persistent().get(&DataKey::Groups(group_id)).expect("Group not found");
        assert!(env.storage().persistent().has(&DataKey::GroupMembers(group_id, member.clone())), "Not a member");
        
        let mut proposal = group.active_proposal.expect("No active proposal");
        assert!(env.ledger().timestamp() < proposal.expiration_timestamp, "Proposal expired");
        
        let has_voted: bool = env.storage().persistent().get(&DataKey::MemberVotes(group_id, member.clone())).unwrap_or(false);
        assert!(!has_voted, "Vote exists");

        if vote_for {
            proposal.votes_for += 1;
        } else {
            proposal.votes_against += 1;
        }

        group.active_proposal = Some(proposal);
        env.storage().persistent().set(&DataKey::MemberVotes(group_id, member), &true);
        env.storage().persistent().set(&DataKey::Groups(group_id), &group);
    }

    pub fn execute_proposal(env: Env, group_id: u32) {
        let mut group: SavingsGroup = env.storage().persistent().get(&DataKey::Groups(group_id)).expect("Group not found");
        let proposal = group.active_proposal.clone().expect("No active proposal");
        
        // Either expired or all members voted
        assert!(env.ledger().timestamp() >= proposal.expiration_timestamp || (proposal.votes_for + proposal.votes_against) == group.member_count, "Cannot execute yet");
        assert!(proposal.votes_for > proposal.votes_against, "Proposal rejected");

        let client = token::Client::new(&env, &group.token);
        client.transfer(&env.current_contract_address(), &proposal.proposer, &proposal.amount);

        group.total_funds -= proposal.amount;
        group.active_proposal = None;
        env.storage().persistent().set(&DataKey::Groups(group_id), &group);
    }
}
