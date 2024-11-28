;; Energy Trading Platform - Main Smart Contracts

;; Utility Token Contract
(define-fungible-token energy-credit)

;; Error Codes
(define-constant ERR-NOT-OWNER (err u1))
(define-constant ERR-LISTING-NOT-FOUND (err u2))
(define-constant ERR-INSUFFICIENT-ENERGY (err u3))
(define-constant ERR-LISTING-EXPIRED (err u4))
(define-constant ERR-INSUFFICIENT-CREDITS (err u5))
(define-constant ERR-ASSET-IN-USE (err u6))
(define-constant ERR-INVALID-EXPIRATION (err u7))
(define-constant ERR-CERTIFICATION-EXPIRED (err u8))
(define-constant ERR-ASSET-NOT-FOUND (err u9))
(define-constant ERR-UNAUTHORIZED (err u10))

;; Initialization
(define-data-var next-asset-id uint u0)
(define-data-var next-listing-id uint u0)

;; Energy Asset Registration Contract
(define-map energy-assets
  { asset-id: uint }
  {
    owner: principal,
    generation-capacity: uint,
    energy-type: (string-ascii 20),
    location: (string-ascii 50),
    certification-date: uint,
    is-active: bool
  }
)

;; Energy Trading Contract
(define-map energy-listings
  { listing-id: uint }
  {
    seller: principal,
    asset-id: uint,
    price-per-kwh: uint,
    available-energy: uint,
    listing-status: (string-ascii 20),
    expiration-date: uint
  }
)

;; Helper function to check if the caller is the owner of a specific asset
(define-private (is-owner (asset-id uint))
  (match (map-get? energy-assets { asset-id: asset-id })
    asset (is-eq tx-sender (get owner asset))
    false
  )
)

;; Helper function to check asset certification expiry
(define-private (is-certification-expired (asset-id uint))
  (match (map-get? energy-assets { asset-id: asset-id })
    asset (> block-height (+ (get certification-date asset) u52560))
    true
  )
)

;; Mint initial energy credits (optional, but often useful)
(define-public (mint-energy-credits (amount uint))
  (ft-mint? energy-credit amount tx-sender)
)

;; Register a new energy asset
(define-public (register-energy-asset
  (generation-capacity uint)
  (energy-type (string-ascii 20))
  (location (string-ascii 50))
)
  (let 
    (
      (asset-id (+ (var-get next-asset-id) u1))
      (asset {
        owner: tx-sender,
        generation-capacity: generation-capacity,
        energy-type: energy-type,
        location: location,
        certification-date: block-height,
        is-active: true
      })
    )
    (map-set energy-assets { asset-id: asset-id } asset)
    (var-set next-asset-id asset-id)
    (print {event: "asset-registered", asset-id: asset-id, owner: tx-sender})
    (ok asset-id)
  )
)

;; Create an energy listing
(define-public (create-energy-listing
  (asset-id uint)
  (price-per-kwh uint)
  (available-energy uint)
  (expiration-date uint)
)
  (let 
    (
      (listing-id (+ (var-get next-listing-id) u1))
      (asset (unwrap! (map-get? energy-assets { asset-id: asset-id }) (err ERR-ASSET-NOT-FOUND)))
    )
    (asserts! (is-owner asset-id) (err ERR-NOT-OWNER))
    (asserts! (get is-active asset) (err ERR-ASSET-IN-USE))
    (asserts! (not (is-certification-expired asset-id)) (err ERR-CERTIFICATION-EXPIRED))
    (asserts! (> expiration-date block-height) (err ERR-INVALID-EXPIRATION))
    
    (map-set energy-listings
      { listing-id: listing-id }
      {
        seller: tx-sender,
        asset-id: asset-id,
        price-per-kwh: price-per-kwh,
        available-energy: available-energy,
        listing-status: "active",
        expiration-date: expiration-date
      }
    )
    (var-set next-listing-id listing-id)
    (print {event: "listing-created", listing-id: listing-id, seller: tx-sender})
    (ok listing-id)
  )
)

;; Update an energy listing
(define-public (update-energy-listing
  (listing-id uint)
  (new-price uint)
  (new-available-energy uint)
)
  (let 
    (
      (listing (unwrap! (map-get? energy-listings { listing-id: listing-id }) (err ERR-LISTING-NOT-FOUND)))
    )
    (asserts! (is-eq tx-sender (get seller listing)) (err ERR-UNAUTHORIZED))
    (asserts! (is-eq (get listing-status listing) "active") (err ERR-LISTING-EXPIRED))
    
    (map-set energy-listings
      { listing-id: listing-id }
      (merge listing {
        price-per-kwh: new-price,
        available-energy: new-available-energy
      })
    )
    (print {event: "listing-updated", listing-id: listing-id, seller: tx-sender})
    (ok true)
  )
)

;; Deactivate a listing
(define-public (deactivate-listing (listing-id uint))
  (let 
    (
      (listing (unwrap! (map-get? energy-listings { listing-id: listing-id }) (err ERR-LISTING-NOT-FOUND)))
    )
    (asserts! (is-eq tx-sender (get seller listing)) (err ERR-UNAUTHORIZED))
    
    (map-set energy-listings
      { listing-id: listing-id }
      (merge listing { listing-status: "inactive" })
    )
    (print {event: "listing-deactivated", listing-id: listing-id, seller: tx-sender})
    (ok true)
  )
)

;; Purchase energy from a listing
(define-public (purchase-energy
  (listing-id uint)
  (energy-amount uint)
)
  (let 
    (
      (listing (unwrap! (map-get? energy-listings { listing-id: listing-id }) (err ERR-LISTING-NOT-FOUND)))
      (seller (get seller listing))
      (total-cost (* (get price-per-kwh listing) energy-amount))
    )
    (asserts! (is-eq (get listing-status listing) "active") (err ERR-LISTING-EXPIRED))
    (asserts! (<= block-height (get expiration-date listing)) (err ERR-LISTING-EXPIRED))
    (asserts! (<= energy-amount (get available-energy listing)) (err ERR-INSUFFICIENT-ENERGY))
    (asserts! (>= (ft-get-balance energy-credit tx-sender) total-cost) (err ERR-INSUFFICIENT-CREDITS))
    
    
    (map-set energy-listings
      { listing-id: listing-id }
      (merge listing {
        available-energy: (- (get available-energy listing) energy-amount)
      })
    )
    (print {event: "energy-purchased", listing-id: listing-id, buyer: tx-sender, amount: energy-amount})
    (ok true)
  )
)

;; Renew asset certification
(define-public (renew-asset-certification (asset-id uint))
  (let 
    (
      (asset (unwrap! (map-get? energy-assets { asset-id: asset-id }) (err ERR-ASSET-NOT-FOUND)))
    )
    (asserts! (is-eq tx-sender (get owner asset)) (err ERR-UNAUTHORIZED))
    
    (map-set energy-assets
      { asset-id: asset-id }
      (merge asset { certification-date: block-height })
    )
    (print {event: "certification-renewed", asset-id: asset-id, owner: tx-sender})
    (ok true)
  )
)

;; Get asset details
(define-read-only (get-asset-details (asset-id uint))
  (map-get? energy-assets { asset-id: asset-id })
)

;; Get listing details
(define-read-only (get-listing-details (listing-id uint))
  (map-get? energy-listings { listing-id: listing-id })
)