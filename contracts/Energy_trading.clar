;; Energy Trading Platform - Main Smart Contracts

;; Utility Token Contract
(define-fungible-token energy-credit)
;; Error Codes
(define-constant ERR-NOT-OWNER (err u1))
(define-constant ERR-LISTING-NOT-FOUND (err u2))
(define-constant ERR-INSUFFICIENT-ENERGY (err u3))

;; Initialization
(define-data-var next-asset-id uint u0)
(define-data-var next-listing-id uint u0)


;; Energy Asset Registration Contract
(define-map energy-assets
  {
    asset-id: uint,
    owner: principal
  }
  {
    generation-capacity: uint,
    energy-type: (string-ascii 20),
    location: (string-ascii 50),
    certification-date: uint
  }
)

;; Energy Trading Contract
(define-map energy-listings
  {
    listing-id: uint,
    seller: principal
  }
  {
    asset-id: uint,
    price-per-kwh: uint,
    available-energy: uint,
    listing-status: (string-ascii 20)
  }
)

;; Helper function to check if the caller is the owner of a specific asset
(define-private (is-owner (asset-id uint))
  (let 
    (
      (asset (unwrap! (map-get? energy-assets 
        {
          asset-id: asset-id, 
          owner: tx-sender
        }
      ) false))
    )
    true
  )
)

;; Mint initial energy credits (optional, but often useful)
(define-public (mint-energy-credits (amount uint))
  (ft-mint? energy-credit amount tx-sender)
)

;; Main Energy Trading Functions

;; Register a new energy asset
(define-public (register-energy-asset
  (generation-capacity uint)
  (energy-type (string-ascii 20))
  (location (string-ascii 50))
)
  (begin
    (let ((asset-id (+ (var-get next-asset-id) u1)))
      (map-set energy-assets 
        {
          asset-id: asset-id, 
          owner: tx-sender
        }
        {
          generation-capacity: generation-capacity,
          energy-type: energy-type,
          location: location,
          certification-date: block-height
        }
      )
      (var-set next-asset-id asset-id)
      (ok asset-id)
    )
  )
)
