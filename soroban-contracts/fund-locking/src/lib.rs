#![no_std]
use soroban_sdk::{contract, contractimpl, contracttype, Address, Env, String, token};

#[contracttype]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LockType {
    Time,
    Event,
    Hybrid,
}

#[contracttype]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LockStatus {
    Active,
    Released,
    Cancelled,
}

#[contracttype]
#[derive(Clone)]
pub struct FundLock {
    pub owner: Address,
    pub token: Address,
    pub amount: i128,
    pub lock_type: LockType,
    pub unlock_timestamp: u64,
    pub event_condition: Option<String>,
    pub status: LockStatus,
    pub beneficiary: Address,
}

#[contracttype]
pub enum DataKey {
    LockCounter,
    Locks(u32),
    EventTrigger(String),
    AuthorizedTrigger(Address),
    Admin,
}

#[contract]
pub struct FundLockContract;

#[contractimpl]
impl FundLockContract {
    pub fn initialize(env: Env, admin: Address) {
        if env.storage().instance().has(&DataKey::Admin) {
            panic!("Already initialized");
        }
        env.storage().instance().set(&DataKey::Admin, &admin);
        env.storage().instance().set(&DataKey::LockCounter, &0u32);
        // Admin is default authorized trigger
        env.storage().persistent().set(&DataKey::AuthorizedTrigger(admin), &true);
    }

    pub fn create_time_lock(
        env: Env,
        owner: Address,
        token: Address,
        amount: i128,
        unlock_timestamp: u64,
        beneficiary: Address,
    ) -> u32 {
        owner.require_auth();
        assert!(amount > 0, "Invalid amount");
        assert!(unlock_timestamp > env.ledger().timestamp(), "Invalid unlock time");

        let client = token::Client::new(&env, &token);
        client.transfer(&owner, &env.current_contract_address(), &amount);

        let counter: u32 = env.storage().instance().get(&DataKey::LockCounter).unwrap_or(0);
        let lock_id = counter + 1;

        let lock = FundLock {
            owner,
            token,
            amount,
            lock_type: LockType::Time,
            unlock_timestamp,
            event_condition: None,
            status: LockStatus::Active,
            beneficiary,
        };

        env.storage().persistent().set(&DataKey::Locks(lock_id), &lock);
        env.storage().instance().set(&DataKey::LockCounter, &lock_id);

        lock_id
    }

    pub fn create_event_lock(
        env: Env,
        owner: Address,
        token: Address,
        amount: i128,
        event_name: String,
        beneficiary: Address,
    ) -> u32 {
        owner.require_auth();
        assert!(amount > 0, "Invalid amount");

        let client = token::Client::new(&env, &token);
        client.transfer(&owner, &env.current_contract_address(), &amount);

        let counter: u32 = env.storage().instance().get(&DataKey::LockCounter).unwrap_or(0);
        let lock_id = counter + 1;

        let lock = FundLock {
            owner,
            token,
            amount,
            lock_type: LockType::Event,
            unlock_timestamp: 0,
            event_condition: Some(event_name),
            status: LockStatus::Active,
            beneficiary,
        };

        env.storage().persistent().set(&DataKey::Locks(lock_id), &lock);
        env.storage().instance().set(&DataKey::LockCounter, &lock_id);

        lock_id
    }

    pub fn create_hybrid_lock(
        env: Env,
        owner: Address,
        token: Address,
        amount: i128,
        unlock_timestamp: u64,
        event_name: String,
        beneficiary: Address,
    ) -> u32 {
        owner.require_auth();
        assert!(amount > 0, "Invalid amount");
        assert!(unlock_timestamp > env.ledger().timestamp(), "Invalid unlock time");

        let client = token::Client::new(&env, &token);
        client.transfer(&owner, &env.current_contract_address(), &amount);

        let counter: u32 = env.storage().instance().get(&DataKey::LockCounter).unwrap_or(0);
        let lock_id = counter + 1;

        let lock = FundLock {
            owner,
            token,
            amount,
            lock_type: LockType::Hybrid,
            unlock_timestamp,
            event_condition: Some(event_name),
            status: LockStatus::Active,
            beneficiary,
        };

        env.storage().persistent().set(&DataKey::Locks(lock_id), &lock);
        env.storage().instance().set(&DataKey::LockCounter, &lock_id);

        lock_id
    }

    pub fn trigger_event(env: Env, caller: Address, event_name: String) {
        caller.require_auth();
        let is_auth = env.storage().persistent().get(&DataKey::AuthorizedTrigger(caller.clone())).unwrap_or(false);
        assert!(is_auth, "Not authorized");

        env.storage().persistent().set(&DataKey::EventTrigger(event_name), &true);
    }

    pub fn release_funds(env: Env, caller: Address, lock_id: u32) {
        caller.require_auth();
        let mut lock: FundLock = env.storage().persistent().get(&DataKey::Locks(lock_id)).expect("Lock not found");
        assert!(caller == lock.owner || caller == lock.beneficiary, "Not authorized");
        assert!(lock.status == LockStatus::Active, "Lock already released or cancelled");

        let is_unlocked = match lock.lock_type {
            LockType::Time => env.ledger().timestamp() >= lock.unlock_timestamp,
            LockType::Event => {
                let event = lock.event_condition.clone().unwrap();
                env.storage().persistent().get(&DataKey::EventTrigger(event)).unwrap_or(false)
            },
            LockType::Hybrid => {
                let time_met = env.ledger().timestamp() >= lock.unlock_timestamp;
                let event = lock.event_condition.clone().unwrap();
                let event_met = env.storage().persistent().get(&DataKey::EventTrigger(event)).unwrap_or(false);
                time_met && event_met
            }
        };

        assert!(is_unlocked, "Lock not expired");

        lock.status = LockStatus::Released;
        env.storage().persistent().set(&DataKey::Locks(lock_id), &lock);

        let client = token::Client::new(&env, &lock.token);
        client.transfer(&env.current_contract_address(), &lock.beneficiary, &lock.amount);
    }
}
