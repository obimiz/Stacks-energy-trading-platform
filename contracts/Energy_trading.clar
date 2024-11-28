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

;; Create an energy listing
(define-public (create-energy-listing
  (asset-id uint)
  (price-per-kwh uint)
  (available-energy uint)
)
  (begin
    (asserts! (is-owner asset-id) (err u1))
    (let ((listing-id (+ (var-get next-listing-id) u1)))
      (map-set energy-listings
        {
          listing-id: listing-id,
          seller: tx-sender
        }
        {
          asset-id: asset-id,
          price-per-kwh: price-per-kwh,
          available-energy: available-energy,
          listing-status: "active"
        }
      )
      (var-set next-listing-id listing-id)
      (ok listing-id)
    )
  )
)

;; Purchase energy from a listing
(define-public (purchase-energy
  (listing-id uint)
  (energy-amount uint)
)
  (begin
    (let 
      (
        (listing-key {
          listing-id: listing-id, 
          seller: tx-sender
        })
        (listing (unwrap! (map-get? energy-listings listing-key) (err u2)))
        (total-cost (* (get price-per-kwh listing) energy-amount))
        (seller (get-seller-from-listing-key listing-key))
      )
      (asserts! (<= energy-amount (get available-energy listing)) (err u3))
      
      ;; Transfer energy credits from buyer to seller
      (try! (ft-transfer? energy-credit total-cost tx-sender seller))
      
      ;; Update listing
      (map-set energy-listings
        listing-key
        (merge listing {
          available-energy: (- (get available-energy listing) energy-amount)
        })
      )
      
      (ok true)
    )
  )
)

;; Helper function to extract seller from listing key
(define-private (get-seller-from-listing-key (key {listing-id: uint, seller: principal}))
  (get seller key)
)

