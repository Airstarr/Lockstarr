
;; title: savings-group
;; version:
;; summary:
;; description:


;; title: savings-group
;; version:
;; summary:
;; description:

;; Savings Groups Smart Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-group-not-found (err u101))
(define-constant err-already-member (err u102))
(define-constant err-not-member (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-vote-exists (err u106))
(define-constant err-no-active-proposal (err u107))
(define-constant err-proposal-expired (err u108))

;; Data Maps
(define-map savings-groups
  { group-id: uint }
  {
    name: (string-ascii 50),
    total-funds: uint,
    member-count: uint,
    active-proposal: (optional {
      proposer: principal,
      amount: uint,
      description: (string-ascii 100),
      votes-for: uint,
      votes-against: uint,
      expiration: uint
    })
  }
)

(define-map group-members
  { group-id: uint, member: principal }
  { joined-at: uint, contribution: uint }
)

(define-map member-votes
  { group-id: uint, member: principal }
  { voted: bool, vote: bool }
)

;; Variables
(define-data-var group-nonce uint u0)

;; Functions

;; Create a new savings group
(define-public (create-group (name (string-ascii 50)))
  (let
    (
      (new-group-id (+ (var-get group-nonce) u1))
    )
    (map-set savings-groups
      { group-id: new-group-id }
      {
        name: name,
        total-funds: u0,
        member-count: u1,
        active-proposal: none
      }
    )
    (map-set group-members
      { group-id: new-group-id, member: tx-sender }
      { joined-at: block-height, contribution: u0 }
    )
    (var-set group-nonce new-group-id)
    (ok new-group-id)
  )
)

;; Join an existing savings group
(define-public (join-group (group-id uint))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) err-group-not-found))
    )
    (asserts! (is-none (map-get? group-members { group-id: group-id, member: tx-sender })) err-already-member)
    (map-set group-members
      { group-id: group-id, member: tx-sender }
      { joined-at: block-height, contribution: u0 }
    )
    (map-set savings-groups
      { group-id: group-id }
      (merge group { member-count: (+ (get member-count group) u1) })
    )
    (ok true)
  )
)

;; Contribute to the group's fund
(define-public (contribute (group-id uint) (amount uint))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) err-group-not-found))
      (member (unwrap! (map-get? group-members { group-id: group-id, member: tx-sender }) err-not-member))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set savings-groups
      { group-id: group-id }
      (merge group { total-funds: (+ (get total-funds group) amount) })
    )
    (map-set group-members
      { group-id: group-id, member: tx-sender }
      (merge member { contribution: (+ (get contribution member) amount) })
    )
    (ok true)
  )
)

;; Create a proposal for fund withdrawal
(define-public (create-proposal (group-id uint) (amount uint) (description (string-ascii 100)))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) err-group-not-found))
    )
    (asserts! (is-some (map-get? group-members { group-id: group-id, member: tx-sender })) err-not-member)
    (asserts! (is-none (get active-proposal group)) err-vote-exists)
    (asserts! (<= amount (get total-funds group)) err-insufficient-funds)
    (map-set savings-groups
      { group-id: group-id }
      (merge group {
        active-proposal: (some {
          proposer: tx-sender,
          amount: amount,
          description: description,
          votes-for: u0,
          votes-against: u0,
          expiration: (+ block-height u144)  ;; Set expiration to 24 hours (assuming 10-minute block times)
        })
      })
    )
    (ok true)
  )
)

;; Vote on an active proposal
(define-public (vote-on-proposal (group-id uint) (vote bool))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) err-group-not-found))
      (proposal (unwrap! (get active-proposal group) err-no-active-proposal))
      (member-vote (default-to { voted: false, vote: false } (map-get? member-votes { group-id: group-id, member: tx-sender })))
    )
    (asserts! (is-some (map-get? group-members { group-id: group-id, member: tx-sender })) err-not-member)
    (asserts! (< block-height (get expiration proposal)) err-proposal-expired)
    (asserts! (not (get voted member-vote)) err-vote-exists)
    (map-set member-votes
      { group-id: group-id, member: tx-sender }
      { voted: true, vote: vote }
    )
    (map-set savings-groups
      { group-id: group-id }
      (merge group {
        active-proposal: (some (merge proposal {
          votes-for: (if vote (+ (get votes-for proposal) u1) (get votes-for proposal)),
          votes-against: (if (not vote) (+ (get votes-against proposal) u1) (get votes-against proposal))
        }))
      })
    )
    (ok true)
  )
)

;; Execute the proposal if it passes
(define-public (execute-proposal (group-id uint))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) err-group-not-found))
      (proposal (unwrap! (get active-proposal group) err-no-active-proposal))
    )
    (asserts! (>= block-height (get expiration proposal)) err-proposal-expired)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get proposer proposal))))
    (map-set savings-groups
      { group-id: group-id }
      (merge group {
        total-funds: (- (get total-funds group) (get amount proposal)),
        active-proposal: none
      })
    )
    (ok true)
  )
)

;; Read-only functions

;; Get group details
(define-read-only (get-group (group-id uint))
  (map-get? savings-groups { group-id: group-id })
)

;; Get member details
(define-read-only (get-member (group-id uint) (member principal))
  (map-get? group-members { group-id: group-id, member: member })
)

;; Get member's vote
(define-read-only (get-member-vote (group-id uint) (member principal))
  (map-get? member-votes { group-id: group-id, member: member })
)

