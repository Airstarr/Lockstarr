#![no_std]
use soroban_sdk::{contract, contractimpl, contracttype, Address, Env, String, Vec};

#[contracttype]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ReminderType {
    Party,
    Contribution,
    FundLock,
    FundUnlock,
    Custom,
}

#[contracttype]
#[derive(Clone)]
pub struct Reminder {
    pub creator: Address,
    pub reminder_type: ReminderType,
    pub title: String,
    pub description: String,
    pub target_timestamp: u64,
    pub recipients: Vec<Address>,
    pub is_active: bool,
}

#[contracttype]
pub enum DataKey {
    Admin,
    ReminderCounter,
    Reminders(u32),
    UserReminders(Address),
    PartyReminders(u32),
    FundLockReminders(u32, ReminderType),
}

#[contract]
pub struct CustomRemindersContract;

#[contractimpl]
impl CustomRemindersContract {
    pub fn initialize(env: Env, admin: Address) {
        if env.storage().instance().has(&DataKey::Admin) {
            panic!("Already initialized");
        }
        env.storage().instance().set(&DataKey::Admin, &admin);
        env.storage().instance().set(&DataKey::ReminderCounter, &0u32);
    }

    fn add_reminder_to_user(env: &Env, user: Address, reminder_id: u32) {
        let mut user_rems: Vec<u32> = env.storage().persistent().get(&DataKey::UserReminders(user.clone())).unwrap_or(Vec::new(&env));
        user_rems.push_back(reminder_id);
        env.storage().persistent().set(&DataKey::UserReminders(user), &user_rems);
    }

    pub fn create_party_reminder(
        env: Env,
        creator: Address,
        party_id: u32,
        title: String,
        description: String,
        target_timestamp: u64,
        recipients: Vec<Address>,
    ) -> u32 {
        creator.require_auth();
        assert!(target_timestamp > env.ledger().timestamp(), "Invalid date");

        let counter: u32 = env.storage().instance().get(&DataKey::ReminderCounter).unwrap_or(0);
        let reminder_id = counter + 1;

        let reminder = Reminder {
            creator,
            reminder_type: ReminderType::Party,
            title,
            description,
            target_timestamp,
            recipients: recipients.clone(),
            is_active: true,
        };

        env.storage().persistent().set(&DataKey::Reminders(reminder_id), &reminder);
        env.storage().instance().set(&DataKey::ReminderCounter, &reminder_id);

        let mut party_rems: Vec<u32> = env.storage().persistent().get(&DataKey::PartyReminders(party_id)).unwrap_or(Vec::new(&env));
        party_rems.push_back(reminder_id);
        env.storage().persistent().set(&DataKey::PartyReminders(party_id), &party_rems);

        for recipient in recipients.iter() {
            Self::add_reminder_to_user(&env, recipient, reminder_id);
        }

        reminder_id
    }

    pub fn create_fund_lock_reminder(
        env: Env,
        creator: Address,
        fund_id: u32,
        is_unlock: bool,
        target_timestamp: u64,
        recipients: Vec<Address>,
    ) -> u32 {
        creator.require_auth();
        assert!(target_timestamp > env.ledger().timestamp(), "Invalid date");
        
        let rem_type = if is_unlock { ReminderType::FundUnlock } else { ReminderType::FundLock };
        assert!(!env.storage().persistent().has(&DataKey::FundLockReminders(fund_id, rem_type.clone())), "Already exists");

        let counter: u32 = env.storage().instance().get(&DataKey::ReminderCounter).unwrap_or(0);
        let reminder_id = counter + 1;

        let title = if is_unlock { String::from_str(&env, "Fund Unlock Date Approaching") } else { String::from_str(&env, "Fund Lock Date Approaching") };
        let description = if is_unlock { String::from_str(&env, "Funds will be unlocked soon.") } else { String::from_str(&env, "Funds will be locked soon.") };

        let reminder = Reminder {
            creator,
            reminder_type: rem_type.clone(),
            title,
            description,
            target_timestamp,
            recipients: recipients.clone(),
            is_active: true,
        };

        env.storage().persistent().set(&DataKey::Reminders(reminder_id), &reminder);
        env.storage().instance().set(&DataKey::ReminderCounter, &reminder_id);
        env.storage().persistent().set(&DataKey::FundLockReminders(fund_id, rem_type), &reminder_id);

        for recipient in recipients.iter() {
            Self::add_reminder_to_user(&env, recipient, reminder_id);
        }

        reminder_id
    }

    pub fn deactivate_reminder(env: Env, caller: Address, reminder_id: u32) {
        caller.require_auth();
        let mut reminder: Reminder = env.storage().persistent().get(&DataKey::Reminders(reminder_id)).expect("Not found");
        let admin: Address = env.storage().instance().get(&DataKey::Admin).unwrap();
        
        assert!(caller == reminder.creator || caller == admin, "Not authorized");
        
        reminder.is_active = false;
        env.storage().persistent().set(&DataKey::Reminders(reminder_id), &reminder);
    }
}
