;; Event Budgeting Smart Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))
(define-constant err-not-found (err u404))
(define-constant err-invalid-amount (err u400))

;; Data Variables
(define-data-var next-event-id uint u1)

;; Data Maps
(define-map events 
  { event-id: uint } 
  {
    name: (string-ascii 100),
    total-budget: uint,
    total-spent: uint
  }
)

(define-map budget-items 
  { event-id: uint, item-name: (string-ascii 50) } 
  {
    budgeted-amount: uint,
    spent-amount: uint,
    vendor: (string-ascii 100)
  }
)

(define-map cost-saving-suggestions
  { event-id: uint, item-name: (string-ascii 50) }
  {
    suggestion: (string-ascii 200),
    estimated-savings: uint
  }
)

;; Private Functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

;; Public Functions

;; Create a new event
(define-public (create-event (name (string-ascii 100)) (total-budget uint))
  (let
    (
      (event-id (var-get next-event-id))
    )
    (asserts! (is-contract-owner) err-unauthorized)
    (asserts! (> total-budget u0) err-invalid-amount)
    
    (map-set events
      { event-id: event-id }
      {
        name: name,
        total-budget: total-budget,
        total-spent: u0
      }
    )
    
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

;; Add a budget item to an event
(define-public (add-budget-item (event-id uint) (item-name (string-ascii 50)) (budgeted-amount uint) (vendor (string-ascii 100)))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) err-not-found))
    )
    (asserts! (is-contract-owner) err-unauthorized)
    (asserts! (> budgeted-amount u0) err-invalid-amount)
    
    (map-set budget-items
      { event-id: event-id, item-name: item-name }
      {
        budgeted-amount: budgeted-amount,
        spent-amount: u0,
        vendor: vendor
      }
    )
    (ok true)
  )
)

;; Track spending for a budget item
(define-public (track-spending (event-id uint) (item-name (string-ascii 50)) (amount uint))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) err-not-found))
      (budget-item (unwrap! (map-get? budget-items { event-id: event-id, item-name: item-name }) err-not-found))
    )
    (asserts! (is-contract-owner) err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set budget-items
      { event-id: event-id, item-name: item-name }
      (merge budget-item { spent-amount: (+ (get spent-amount budget-item) amount) })
    )
    
    (map-set events
      { event-id: event-id }
      (merge event { total-spent: (+ (get total-spent event) amount) })
    )
    
    (ok true)
  )
)

;; Add a cost-saving suggestion
(define-public (add-cost-saving-suggestion (event-id uint) (item-name (string-ascii 50)) (suggestion (string-ascii 200)) (estimated-savings uint))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) err-not-found))
      (budget-item (unwrap! (map-get? budget-items { event-id: event-id, item-name: item-name }) err-not-found))
    )
    (asserts! (is-contract-owner) err-unauthorized)
    (asserts! (> estimated-savings u0) err-invalid-amount)
    
    (map-set cost-saving-suggestions
      { event-id: event-id, item-name: item-name }
      {
        suggestion: suggestion,
        estimated-savings: estimated-savings
      }
    )
    (ok true)
  )
)

;; Read-only Functions

;; Get event details
(define-read-only (get-event-details (event-id uint))
  (map-get? events { event-id: event-id })
)

;; Get budget item details
(define-read-only (get-budget-item-details (event-id uint) (item-name (string-ascii 50)))
  (map-get? budget-items { event-id: event-id, item-name: item-name })
)

;; Get cost-saving suggestion
(define-read-only (get-cost-saving-suggestion (event-id uint) (item-name (string-ascii 50)))
  (map-get? cost-saving-suggestions { event-id: event-id, item-name: item-name })
)

;; Get remaining budget for an event
(define-read-only (get-remaining-budget (event-id uint))
  (match (map-get? events { event-id: event-id })
    event (ok (- (get total-budget event) (get total-spent event)))
    err-not-found
  )
)