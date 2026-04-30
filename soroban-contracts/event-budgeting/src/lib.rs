#![no_std]
use soroban_sdk::{contract, contractimpl, contracttype, Address, Env, String};

#[contracttype]
#[derive(Clone)]
pub struct Event {
    pub name: String,
    pub total_budget: i128,
    pub total_spent: i128,
}

#[contracttype]
#[derive(Clone)]
pub struct BudgetItem {
    pub budgeted_amount: i128,
    pub spent_amount: i128,
    pub vendor: String,
}

#[contracttype]
#[derive(Clone)]
pub struct CostSavingSuggestion {
    pub suggestion: String,
    pub estimated_savings: i128,
}

#[contracttype]
pub enum DataKey {
    Admin,
    NextEventId,
    Events(u32),
    BudgetItems(u32, String),
    CostSavings(u32, String),
}

#[contract]
pub struct EventBudgetingContract;

#[contractimpl]
impl EventBudgetingContract {
    pub fn initialize(env: Env, admin: Address) {
        if env.storage().instance().has(&DataKey::Admin) {
            panic!("Already initialized");
        }
        env.storage().instance().set(&DataKey::Admin, &admin);
        env.storage().instance().set(&DataKey::NextEventId, &1u32);
    }

    fn check_admin(env: &Env, caller: &Address) {
        caller.require_auth();
        let admin: Address = env.storage().instance().get(&DataKey::Admin).expect("Not initialized");
        assert!(admin == *caller, "Not authorized");
    }

    pub fn create_event(env: Env, caller: Address, name: String, total_budget: i128) -> u32 {
        Self::check_admin(&env, &caller);
        assert!(total_budget > 0, "Invalid amount");

        let event_id: u32 = env.storage().instance().get(&DataKey::NextEventId).unwrap();
        
        let event = Event {
            name,
            total_budget,
            total_spent: 0,
        };

        env.storage().persistent().set(&DataKey::Events(event_id), &event);
        env.storage().instance().set(&DataKey::NextEventId, &(event_id + 1));

        event_id
    }

    pub fn add_budget_item(env: Env, caller: Address, event_id: u32, item_name: String, budgeted_amount: i128, vendor: String) {
        Self::check_admin(&env, &caller);
        assert!(budgeted_amount > 0, "Invalid amount");
        assert!(env.storage().persistent().has(&DataKey::Events(event_id)), "Event not found");

        let budget_item = BudgetItem {
            budgeted_amount,
            spent_amount: 0,
            vendor,
        };

        env.storage().persistent().set(&DataKey::BudgetItems(event_id, item_name), &budget_item);
    }

    pub fn track_spending(env: Env, caller: Address, event_id: u32, item_name: String, amount: i128) {
        Self::check_admin(&env, &caller);
        assert!(amount > 0, "Invalid amount");

        let mut event: Event = env.storage().persistent().get(&DataKey::Events(event_id)).expect("Event not found");
        let mut item: BudgetItem = env.storage().persistent().get(&DataKey::BudgetItems(event_id, item_name.clone())).expect("Item not found");

        item.spent_amount += amount;
        event.total_spent += amount;

        env.storage().persistent().set(&DataKey::BudgetItems(event_id, item_name), &item);
        env.storage().persistent().set(&DataKey::Events(event_id), &event);
    }

    pub fn add_cost_saving_suggestion(env: Env, caller: Address, event_id: u32, item_name: String, suggestion: String, estimated_savings: i128) {
        Self::check_admin(&env, &caller);
        assert!(estimated_savings > 0, "Invalid amount");
        assert!(env.storage().persistent().has(&DataKey::Events(event_id)), "Event not found");
        assert!(env.storage().persistent().has(&DataKey::BudgetItems(event_id, item_name.clone())), "Item not found");

        let cost_saving = CostSavingSuggestion {
            suggestion,
            estimated_savings,
        };

        env.storage().persistent().set(&DataKey::CostSavings(event_id, item_name), &cost_saving);
    }
}
