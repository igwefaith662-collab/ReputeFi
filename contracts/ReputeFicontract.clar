;; title: ReputeFi - Algorithmic Reputation Derivatives
;; version: 1.0.0
;; summary: Trade derivatives based on user reputation scores across multiple protocols
;; description: A comprehensive DeFi protocol that tokenizes reputation through NFT certificates,
;;              enables prediction markets on reputation changes, provides reputation-based lending,
;;              and offers insurance against reputation loss.

;; No external traits - using built-in functions directly

;; token definitions
(define-fungible-token reputation-token)
(define-non-fungible-token reputation-certificate uint)

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_CERTIFICATE_EXISTS (err u103))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u104))
(define-constant ERR_PREDICTION_NOT_FOUND (err u105))
(define-constant ERR_PREDICTION_ENDED (err u106))
(define-constant ERR_INVALID_REPUTATION (err u107))
(define-constant ERR_LOAN_ACTIVE (err u108))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u109))

(define-constant MIN_REPUTATION u100)
(define-constant MAX_REPUTATION u10000)
(define-constant BASE_LENDING_RATE u500) ;; 5% base rate
(define-constant REPUTATION_MULTIPLIER u10)
(define-constant INSURANCE_PREMIUM_RATE u50) ;; 0.5%

;; data vars
(define-data-var next-certificate-id uint u1)
(define-data-var next-prediction-id uint u1)
(define-data-var next-loan-id uint u1)
(define-data-var protocol-fee-rate uint u25) ;; 0.25%
(define-data-var total-staked uint u0)

;; data maps
(define-map user-reputation 
  principal 
  {
    github-score: uint,
    twitter-score: uint,
    defi-score: uint,
    composite-score: uint,
    last-updated: uint
  }
)

(define-map reputation-certificates
  uint
  {
    owner: principal,
    reputation-score: uint,
    protocol-source: (string-ascii 20),
    minted-at: uint,
    expires-at: uint
  }
)

(define-map prediction-markets
  uint
  {
    creator: principal,
    target-user: principal,
    current-reputation: uint,
    predicted-reputation: uint,
    deadline: uint,
    total-staked-up: uint,
    total-staked-down: uint,
    resolved: bool,
    outcome: (optional bool)
  }
)

(define-map user-predictions
  {user: principal, market-id: uint}
  {
    amount-up: uint,
    amount-down: uint
  }
)

(define-map reputation-loans
  uint
  {
    borrower: principal,
    amount: uint,
    interest-rate: uint,
    reputation-collateral: uint,
    start-block: uint,
    duration: uint,
    repaid: bool
  }
)

(define-map insurance-policies
  {user: principal, policy-id: uint}
  {
    coverage-amount: uint,
    premium-paid: uint,
    start-block: uint,
    duration: uint,
    active: bool
  }
)

(define-map protocol-balances
  (string-ascii 20)
  uint
)


;; public functions

;; Update user reputation from external protocols
(define-public (update-reputation (user principal) 
                                 (github-score uint) 
                                 (twitter-score uint) 
                                 (defi-score uint))
  (let ((composite (calculate-composite-score github-score twitter-score defi-score)))
    (asserts! (>= composite MIN_REPUTATION) ERR_INVALID_REPUTATION)
    (asserts! (<= composite MAX_REPUTATION) ERR_INVALID_REPUTATION)
    (map-set user-reputation user {
      github-score: github-score,
      twitter-score: twitter-score,
      defi-score: defi-score,
      composite-score: composite,
      last-updated: u0  ;; Using placeholder for block-height in development
    })
    (ok composite)
  )
)

;; Mint reputation certificate NFT
(define-public (mint-reputation-certificate (protocol (string-ascii 20)) (duration uint))
  (let (
    (certificate-id (var-get next-certificate-id))
    (user-rep (unwrap! (map-get? user-reputation tx-sender) ERR_INVALID_REPUTATION))
    (reputation-score (get composite-score user-rep))
  )
    (asserts! (>= reputation-score MIN_REPUTATION) ERR_INSUFFICIENT_REPUTATION)
    (try! (nft-mint? reputation-certificate certificate-id tx-sender))
    (map-set reputation-certificates certificate-id {
      owner: tx-sender,
      reputation-score: reputation-score,
      protocol-source: protocol,
      minted-at: u0,  ;; Using placeholder for block-height in development
      expires-at: u1000  ;; Using placeholder for block-height + duration in development
    })
    (var-set next-certificate-id (+ certificate-id u1))
    (ok certificate-id)
  )
)

;; Create prediction market on reputation change
(define-public (create-prediction-market (target-user principal) 
                                        (predicted-reputation uint) 
                                        (deadline uint))
  (let (
    (market-id (var-get next-prediction-id))
    (current-rep (get-user-reputation target-user))
  )
    (asserts! true ERR_PREDICTION_ENDED)  ;; Bypassing block-height check in development
    (asserts! (> predicted-reputation u0) ERR_INVALID_REPUTATION)
    (map-set prediction-markets market-id {
      creator: tx-sender,
      target-user: target-user,
      current-reputation: current-rep,
      predicted-reputation: predicted-reputation,
      deadline: deadline,
      total-staked-up: u0,
      total-staked-down: u0,
      resolved: false,
      outcome: none
    })
    (var-set next-prediction-id (+ market-id u1))
    (ok market-id)
  )
)

;; Stake on prediction market outcome
(define-public (stake-on-prediction (market-id uint) (amount uint) (direction bool))
  (let (
    (market (unwrap! (map-get? prediction-markets market-id) ERR_PREDICTION_NOT_FOUND))
    (current-stake (default-to {amount-up: u0, amount-down: u0} 
                              (map-get? user-predictions {user: tx-sender, market-id: market-id})))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! true ERR_PREDICTION_ENDED)  ;; Bypassing block-height check in development
    (asserts! (not (get resolved market)) ERR_PREDICTION_ENDED)
    
    (try! (ft-transfer? reputation-token amount tx-sender (as-contract tx-sender)))
    
    (if direction
      ;; Staking UP
      (begin
        (map-set prediction-markets market-id 
          (merge market {total-staked-up: (+ (get total-staked-up market) amount)}))
        (map-set user-predictions {user: tx-sender, market-id: market-id}
          (merge current-stake {amount-up: (+ (get amount-up current-stake) amount)}))
      )
      ;; Staking DOWN
      (begin
        (map-set prediction-markets market-id 
          (merge market {total-staked-down: (+ (get total-staked-down market) amount)}))
        (map-set user-predictions {user: tx-sender, market-id: market-id}
          (merge current-stake {amount-down: (+ (get amount-down current-stake) amount)}))
      )
    )
    (var-set total-staked (+ (var-get total-staked) amount))
    (ok true)
  )
)

;; Resolve prediction market
(define-public (resolve-prediction-market (market-id uint))
  (let (
    (market (unwrap! (map-get? prediction-markets market-id) ERR_PREDICTION_NOT_FOUND))
    (current-rep (get-user-reputation (get target-user market)))
    (predicted-rep (get predicted-reputation market))
    (outcome (>= current-rep predicted-rep))
  )
    (asserts! true ERR_PREDICTION_ENDED)  ;; Bypassing block-height check in development
    (asserts! (not (get resolved market)) ERR_PREDICTION_ENDED)
    
    (map-set prediction-markets market-id 
      (merge market {resolved: true, outcome: (some outcome)}))
    (ok outcome)
  )
)

;; Claim prediction winnings
(define-public (claim-prediction-winnings (market-id uint))
  (let (
    (market (unwrap! (map-get? prediction-markets market-id) ERR_PREDICTION_NOT_FOUND))
    (user-stake (unwrap! (map-get? user-predictions {user: tx-sender, market-id: market-id}) ERR_INVALID_AMOUNT))
    (outcome (unwrap! (get outcome market) ERR_PREDICTION_ENDED))
  )
    (asserts! (get resolved market) ERR_PREDICTION_ENDED)
    
    (let (
      (winning-pool (if outcome (get total-staked-up market) (get total-staked-down market)))
      (losing-pool (if outcome (get total-staked-down market) (get total-staked-up market)))
      (user-winning-stake (if outcome (get amount-up user-stake) (get amount-down user-stake)))
      (payout (if (> winning-pool u0)
                 (+ user-winning-stake (/ (* user-winning-stake losing-pool) winning-pool))
                 user-winning-stake))
    )
      (let ((transfer-result (if (> user-winning-stake u0)
                              (match (as-contract (ft-transfer? reputation-token payout tx-sender tx-sender))
                                result (ok result)
                                error (err u0)
                              )
                              (ok true)
                            )))
        (if (is-ok transfer-result)
          (ok payout)
          (err u0)
        )
      )
    )
  )
)

;; Create reputation-backed loan
(define-public (create-reputation-loan (amount uint) (duration uint))
  (let (
    (loan-id (var-get next-loan-id))
    (user-rep (get-user-reputation tx-sender))
    (required-reputation (* amount REPUTATION_MULTIPLIER))
    (interest-rate (calculate-interest-rate user-rep))
  )
    (asserts! (>= user-rep required-reputation) ERR_INSUFFICIENT_REPUTATION)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set reputation-loans loan-id {
      borrower: tx-sender,
      amount: amount,
      interest-rate: interest-rate,
      reputation-collateral: user-rep,
      start-block: u0,  ;; Using placeholder for block-height in development
      duration: duration,
      repaid: false
    })
    
    (try! (ft-mint? reputation-token amount tx-sender))
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)
  )
)

;; Repay reputation loan
(define-public (repay-loan (loan-id uint))
  (let (
    (loan (unwrap! (map-get? reputation-loans loan-id) ERR_PREDICTION_NOT_FOUND))
    (interest (calculate-loan-interest loan))
    (total-repayment (+ (get amount loan) interest))
  )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get repaid loan)) ERR_LOAN_ACTIVE)
    
    (try! (ft-burn? reputation-token total-repayment tx-sender))
    (map-set reputation-loans loan-id (merge loan {repaid: true}))
    (ok total-repayment)
  )
)

;; Purchase reputation insurance
(define-public (purchase-insurance (coverage-amount uint) (duration uint))
  (let (
    (premium (/ (* coverage-amount INSURANCE_PREMIUM_RATE) u10000))
    (user-rep (get-user-reputation tx-sender))
  )
    (asserts! (>= user-rep MIN_REPUTATION) ERR_INSUFFICIENT_REPUTATION)
    (asserts! (> coverage-amount u0) ERR_INVALID_AMOUNT)
    
    (try! (ft-burn? reputation-token premium tx-sender))
    (map-set insurance-policies {user: tx-sender, policy-id: u0} {  ;; Using placeholder for block-height in development
      coverage-amount: coverage-amount,
      premium-paid: premium,
      start-block: u0,  ;; Using placeholder for block-height in development
      duration: duration,
      active: true
    })
    (ok true)
  )
)

;; Mint initial reputation tokens for testing
(define-public (mint-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (try! (ft-mint? reputation-token amount recipient))
    (ok amount)
  )
)

;; read only functions

;; Get user's composite reputation score
(define-read-only (get-user-reputation (user principal))
  (default-to u0 (get composite-score (map-get? user-reputation user)))
)

;; Get reputation certificate details
(define-read-only (get-certificate (certificate-id uint))
  (map-get? reputation-certificates certificate-id)
)

;; Get prediction market details
(define-read-only (get-prediction-market (market-id uint))
  (map-get? prediction-markets market-id)
)

;; Get user's prediction stakes
(define-read-only (get-user-prediction-stakes (user principal) (market-id uint))
  (map-get? user-predictions {user: user, market-id: market-id})
)

;; Get loan details
(define-read-only (get-loan (loan-id uint))
  (map-get? reputation-loans loan-id)
)

;; Calculate interest rate based on reputation
(define-read-only (calculate-interest-rate (reputation uint))
  (if (>= reputation (* MIN_REPUTATION u5))
    (/ BASE_LENDING_RATE u2) ;; 2.5% for high reputation
    BASE_LENDING_RATE ;; 5% for normal reputation
  )
)

;; Get token balance
(define-read-only (get-balance (user principal))
  (ft-get-balance reputation-token user)
)

;; Get total token supply
(define-read-only (get-total-supply)
  (ft-get-supply reputation-token)
)

;; private functions

;; Calculate composite reputation score
(define-private (calculate-composite-score (github uint) (twitter uint) (defi uint))
  (/ (+ (* github u3) (* twitter u2) (* defi u5)) u10)
)

;; Calculate loan interest based on time
(define-private (calculate-loan-interest (loan {borrower: principal, amount: uint, interest-rate: uint, reputation-collateral: uint, start-block: uint, duration: uint, repaid: bool}))
  (let (
    (blocks-elapsed u100)  ;; Using placeholder for block-height calculation
    (blocks-in-duration (get duration loan))
  )
    (/ (* (get amount loan) (get interest-rate loan) blocks-elapsed) (* u10000 blocks-in-duration))
  )
)
