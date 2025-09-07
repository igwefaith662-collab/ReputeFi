;; title: ReputeFi - Algorithmic Reputation Derivatives
;; version: 1.0.0
;; summary: Trade derivatives based on user reputation scores across multiple protocols
;; description: A comprehensive DeFi protocol that tokenizes reputation through NFT certificates,
;;              enables prediction markets on reputation changes, provides reputation-based lending,
;;              and offers insurance against reputation loss.

;; traits
(use-trait sip-010-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)
(use-trait sip-009-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

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
