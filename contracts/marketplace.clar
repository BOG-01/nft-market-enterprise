;; NFT Marketplace Contract
;; A comprehensive marketplace for trading NFTs with advanced features

;; ==================== CONSTANTS ====================

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-PRICE (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-NOT-OWNER (err u105))
(define-constant ERR-NOT-FOR-SALE (err u106))
(define-constant ERR-SELF-PURCHASE (err u107))
(define-constant ERR-INVALID-PARAMETERS (err u108))
(define-constant ERR-AUCTION-ACTIVE (err u109))
(define-constant ERR-AUCTION-ENDED (err u110))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MARKETPLACE-FEE u250) ;; 2.5% fee (250 basis points)
(define-constant MAX-ROYALTY u1000) ;; 10% max royalty
(define-constant MIN-AUCTION-DURATION u144) ;; ~24 hours in blocks
(define-constant MAX-AUCTION-DURATION u4320) ;; ~30 days in blocks

;; NFT Collection Info
(define-constant COLLECTION-NAME "NFT Marketplace Collection")
(define-constant COLLECTION-SYMBOL "NFTMC")

;; ==================== DATA MAPS AND VARIABLES ====================

;; NFT ownership and metadata
(define-map nft-owners uint principal)
(define-map nft-metadata uint {
    name: (string-ascii 64),
    description: (string-ascii 256),
    image-uri: (string-ascii 256),
    creator: principal,
    royalty: uint
})

;; Marketplace listings
(define-map listings uint {
    seller: principal,
    price: uint,
    listed-at: uint
})

;; Auction data
(define-map auctions uint {
    seller: principal,
    min-bid: uint,
    current-bid: uint,
    current-bidder: (optional principal),
    end-block: uint,
    reserve-met: bool
})

;; Offers on NFTs
(define-map offers {nft-id: uint, buyer: principal} {
    amount: uint,
    expires-at: uint
})

;; Collection statistics
(define-map collection-stats principal {
    total-minted: uint,
    total-sold: uint,
    total-volume: uint
})

;; User profiles
(define-map user-profiles principal {
    username: (string-ascii 32),
    bio: (string-ascii 256),
    avatar-uri: (string-ascii 256),
    verified: bool
})

;; Contract state variables
(define-data-var next-token-id uint u1)
(define-data-var marketplace-enabled bool true)
(define-data-var contract-uri (string-ascii 256) "https://nft-marketplace.com/contract")

;; Revenue tracking
(define-data-var total-marketplace-revenue uint u0)
(define-data-var total-creator-royalties uint u0)

;; ==================== PRIVATE HELPER FUNCTIONS ====================

;; Calculate marketplace fee
(define-private (calculate-marketplace-fee (price uint))
    (/ (* price MARKETPLACE-FEE) u10000)
)

;; Calculate royalty amount
(define-private (calculate-royalty (price uint) (royalty-rate uint))
    (/ (* price royalty-rate) u10000)
)

;; Check if caller is contract owner
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT-OWNER)
)

;; Validate royalty rate
(define-private (is-valid-royalty (royalty uint))
    (<= royalty MAX-ROYALTY)
)

;; Check if marketplace is enabled
(define-private (is-marketplace-enabled)
    (var-get marketplace-enabled)
)

;; Get current block height
(define-private (get-current-block)
    block-height
)

;; ==================== READ-ONLY FUNCTIONS ====================

;; Get next token ID
(define-read-only (get-next-token-id)
    (var-get next-token-id)
)

;; Check if marketplace is enabled
(define-read-only (get-marketplace-status)
    (var-get marketplace-enabled)
)

;; Get contract URI
(define-read-only (get-contract-uri)
    (var-get contract-uri)
)

;; Get total marketplace revenue
(define-read-only (get-total-revenue)
    (var-get total-marketplace-revenue)
)

;; Get total creator royalties paid
(define-read-only (get-total-royalties)
    (var-get total-creator-royalties)
)

;; Get marketplace fee rate
(define-read-only (get-marketplace-fee-rate)
    MARKETPLACE-FEE
)

;; Get collection info
(define-read-only (get-collection-info)
    {
        name: COLLECTION-NAME,
        symbol: COLLECTION-SYMBOL,
        total-supply: (- (var-get next-token-id) u1)
    }
)

;; ==================== CORE NFT FUNCTIONS ====================

;; Get owner of NFT
(define-read-only (get-owner (token-id uint))
    (map-get? nft-owners token-id)
)

;; Get NFT metadata
(define-read-only (get-metadata (token-id uint))
    (map-get? nft-metadata token-id)
)

;; Get NFT URI (for compatibility)
(define-read-only (get-token-uri (token-id uint))
    (match (map-get? nft-metadata token-id)
        metadata (ok (some (get image-uri metadata)))
        (ok none)
    )
)

;; Check if NFT exists
(define-read-only (nft-exists (token-id uint))
    (is-some (map-get? nft-owners token-id))
)

;; Get all NFTs owned by a principal
(define-read-only (get-balance (owner principal))
    (default-to u0 
        (get total-minted (map-get? collection-stats owner))
    )
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user)
)

;; ==================== MINTING FUNCTIONS ====================

;; Mint new NFT
(define-public (mint-nft 
    (recipient principal)
    (name (string-ascii 64))
    (description (string-ascii 256))
    (image-uri (string-ascii 256))
    (royalty uint)
)
    (let (
        (token-id (var-get next-token-id))
    )
        ;; Validate inputs
        (asserts! (> (len name) u0) ERR-INVALID-PARAMETERS)
        (asserts! (> (len image-uri) u0) ERR-INVALID-PARAMETERS)
        (asserts! (is-valid-royalty royalty) ERR-INVALID-PARAMETERS)
        
        ;; Store NFT ownership
        (map-set nft-owners token-id recipient)
        
        ;; Store NFT metadata
        (map-set nft-metadata token-id {
            name: name,
            description: description,
            image-uri: image-uri,
            creator: tx-sender,
            royalty: royalty
        })
        
        ;; Update collection stats for creator
        (map-set collection-stats tx-sender
            (merge 
                (default-to {total-minted: u0, total-sold: u0, total-volume: u0}
                    (map-get? collection-stats tx-sender)
                )
                {total-minted: (+ (default-to u0 
                    (get total-minted (map-get? collection-stats tx-sender))) u1)}
            )
        )
        
        ;; Increment token ID
        (var-set next-token-id (+ token-id u1))
        
        ;; Print mint event
        (print {
            action: "mint",
            token-id: token-id,
            recipient: recipient,
            creator: tx-sender
        })
        
        (ok token-id)
    )
)

;; Batch mint multiple NFTs
(define-public (batch-mint 
    (recipients (list 10 principal))
    (names (list 10 (string-ascii 64)))
    (descriptions (list 10 (string-ascii 256)))
    (image-uris (list 10 (string-ascii 256)))
    (royalty uint)
)
    (let (
        (batch-size (len recipients))
    )
        ;; Validate all lists have same length
        (asserts! (is-eq batch-size (len names)) ERR-INVALID-PARAMETERS)
        (asserts! (is-eq batch-size (len descriptions)) ERR-INVALID-PARAMETERS)
        (asserts! (is-eq batch-size (len image-uris)) ERR-INVALID-PARAMETERS)
        (asserts! (> batch-size u0) ERR-INVALID-PARAMETERS)
        (asserts! (is-valid-royalty royalty) ERR-INVALID-PARAMETERS)
        
        ;; Mint each NFT in the batch
        (ok (map mint-single-in-batch 
            recipients names descriptions image-uris
        ))
    )
)

;; Helper function for batch minting
(define-private (mint-single-in-batch 
    (recipient principal)
    (name (string-ascii 64))
    (description (string-ascii 256))
    (image-uri (string-ascii 256))
)
    (let (
        (token-id (var-get next-token-id))
    )
        ;; Store NFT data
        (map-set nft-owners token-id recipient)
        (map-set nft-metadata token-id {
            name: name,
            description: description,
            image-uri: image-uri,
            creator: tx-sender,
            royalty: u0 ;; Default royalty for batch mint
        })
        
        ;; Increment token ID
        (var-set next-token-id (+ token-id u1))
        token-id
    )
)

;; ==================== TRANSFER FUNCTIONS ====================

;; Transfer NFT
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (let (
        (current-owner (unwrap! (map-get? nft-owners token-id) ERR-NOT-FOUND))
    )
        ;; Validate transfer
        (asserts! (is-eq current-owner sender) ERR-NOT-OWNER)
        (asserts! (or (is-eq tx-sender sender) (is-eq tx-sender current-owner)) ERR-UNAUTHORIZED)
        (asserts! (not (is-eq sender recipient)) ERR-INVALID-PARAMETERS)
        
        ;; Remove from any active listings
        (map-delete listings token-id)
        
        ;; Update ownership
        (map-set nft-owners token-id recipient)
        
        ;; Print transfer event
        (print {
            action: "transfer",
            token-id: token-id,
            sender: sender,
            recipient: recipient
        })
        
        (ok true)
    )
)

;; Safe transfer (same as transfer for this implementation)
(define-public (safe-transfer (token-id uint) (sender principal) (recipient principal))
    (transfer token-id sender recipient)
)

;; ==================== APPROVAL FUNCTIONS ====================

;; Set user profile
(define-public (set-user-profile 
    (username (string-ascii 32))
    (bio (string-ascii 256))
    (avatar-uri (string-ascii 256))
)
    (begin
        (map-set user-profiles tx-sender {
            username: username,
            bio: bio,
            avatar-uri: avatar-uri,
            verified: false
        })
        
        (print {
            action: "profile-updated",
            user: tx-sender,
            username: username
        })
        
        (ok true)
    )
)

;; Verify user (admin only)
(define-public (verify-user (user principal))
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        
        (match (map-get? user-profiles user)
            profile (begin
                (map-set user-profiles user
                    (merge profile {verified: true})
                )
                (ok true)
            )
            ERR-NOT-FOUND
        )
    )
)

;; Update metadata (creator only)
(define-public (update-metadata 
    (token-id uint)
    (name (string-ascii 64))
    (description (string-ascii 256))
    (image-uri (string-ascii 256))
)
    (let (
        (metadata (unwrap! (map-get? nft-metadata token-id) ERR-NOT-FOUND))
        (creator (get creator metadata))
    )
        ;; Only creator can update metadata
        (asserts! (is-eq tx-sender creator) ERR-UNAUTHORIZED)
        (asserts! (> (len name) u0) ERR-INVALID-PARAMETERS)
        (asserts! (> (len image-uri) u0) ERR-INVALID-PARAMETERS)
        
        ;; Update metadata while preserving creator and royalty
        (map-set nft-metadata token-id
            (merge metadata {
                name: name,
                description: description,
                image-uri: image-uri
            })
        )
        
        (print {
            action: "metadata-updated",
            token-id: token-id,
            creator: creator
        })
        
        (ok true)
    )
)

;; ==================== MARKETPLACE FUNCTIONS ====================

;; List NFT for sale
(define-public (list-nft (token-id uint) (price uint))
    (let (
        (owner (unwrap! (map-get? nft-owners token-id) ERR-NOT-FOUND))
    )
        ;; Validate listing
        (asserts! (is-marketplace-enabled) ERR-UNAUTHORIZED)
        (asserts! (is-eq tx-sender owner) ERR-NOT-OWNER)
        (asserts! (> price u0) ERR-INVALID-PRICE)
        (asserts! (is-none (map-get? listings token-id)) ERR-ALREADY-EXISTS)
        
        ;; Create listing
        (map-set listings token-id {
            seller: owner,
            price: price,
            listed-at: (get-current-block)
        })
        
        (print {
            action: "listed",
            token-id: token-id,
            seller: owner,
            price: price
        })
        
        (ok true)
    )
)

;; Update listing price
(define-public (update-listing-price (token-id uint) (new-price uint))
    (let (
        (listing (unwrap! (map-get? listings token-id) ERR-NOT-FOR-SALE))
        (seller (get seller listing))
    )
        ;; Validate update
        (asserts! (is-eq tx-sender seller) ERR-NOT-OWNER)
        (asserts! (> new-price u0) ERR-INVALID-PRICE)
        
        ;; Update listing
        (map-set listings token-id
            (merge listing {price: new-price})
        )
        
        (print {
            action: "price-updated",
            token-id: token-id,
            seller: seller,
            old-price: (get price listing),
            new-price: new-price
        })
        
        (ok true)
    )
)

;; Remove listing
(define-public (unlist-nft (token-id uint))
    (let (
        (listing (unwrap! (map-get? listings token-id) ERR-NOT-FOR-SALE))
        (seller (get seller listing))
    )
        ;; Validate unlisting
        (asserts! (is-eq tx-sender seller) ERR-NOT-OWNER)
        
        ;; Remove listing
        (map-delete listings token-id)
        
        (print {
            action: "unlisted",
            token-id: token-id,
            seller: seller
        })
        
        (ok true)
    )
)

;; Buy NFT
(define-public (buy-nft (token-id uint))
    (let (
        (listing (unwrap! (map-get? listings token-id) ERR-NOT-FOR-SALE))
        (seller (get seller listing))
        (price (get price listing))
        (metadata (unwrap! (map-get? nft-metadata token-id) ERR-NOT-FOUND))
        (creator (get creator metadata))
        (royalty-rate (get royalty metadata))
        (marketplace-fee (calculate-marketplace-fee price))
        (royalty-amount (calculate-royalty price royalty-rate))
        (seller-amount (- (- price marketplace-fee) royalty-amount))
    )
        ;; Validate purchase
        (asserts! (is-marketplace-enabled) ERR-UNAUTHORIZED)
        (asserts! (not (is-eq tx-sender seller)) ERR-SELF-PURCHASE)
        (asserts! (>= (stx-get-balance tx-sender) price) ERR-INSUFFICIENT-FUNDS)
        
        ;; Transfer STX payments
        (try! (stx-transfer? seller-amount tx-sender seller))
        (try! (stx-transfer? marketplace-fee tx-sender CONTRACT-OWNER))
        
        ;; Pay royalties to creator (if different from seller)
        (if (and (> royalty-amount u0) (not (is-eq creator seller)))
            (try! (stx-transfer? royalty-amount tx-sender creator))
            true
        )
        
        ;; Transfer NFT ownership
        (map-set nft-owners token-id tx-sender)
        
        ;; Remove listing
        (map-delete listings token-id)
        
        ;; Update statistics
        (var-set total-marketplace-revenue 
            (+ (var-get total-marketplace-revenue) marketplace-fee)
        )
        (var-set total-creator-royalties 
            (+ (var-get total-creator-royalties) royalty-amount)
        )
        
        ;; Update seller stats
        (map-set collection-stats seller
            (merge 
                (default-to {total-minted: u0, total-sold: u0, total-volume: u0}
                    (map-get? collection-stats seller)
                )
                {
                    total-sold: (+ (default-to u0 
                        (get total-sold (map-get? collection-stats seller))) u1),
                    total-volume: (+ (default-to u0 
                        (get total-volume (map-get? collection-stats seller))) price)
                }
            )
        )
        
        (print {
            action: "purchased",
            token-id: token-id,
            buyer: tx-sender,
            seller: seller,
            price: price,
            marketplace-fee: marketplace-fee,
            royalty-amount: royalty-amount
        })
        
        (ok true)
    )
)

;; Get listing information
(define-read-only (get-listing (token-id uint))
    (map-get? listings token-id)
)

;; Check if NFT is listed
(define-read-only (is-listed (token-id uint))
    (is-some (map-get? listings token-id))
)

;; ==================== OFFER FUNCTIONS ====================

;; Make offer on NFT
(define-public (make-offer (token-id uint) (amount uint) (expires-at uint))
    (begin
        ;; Validate offer
        (asserts! (nft-exists token-id) ERR-NOT-FOUND)
        (asserts! (> amount u0) ERR-INVALID-PRICE)
        (asserts! (> expires-at (get-current-block)) ERR-INVALID-PARAMETERS)
        (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
        
        ;; Store offer
        (map-set offers {nft-id: token-id, buyer: tx-sender} {
            amount: amount,
            expires-at: expires-at
        })
        
        (print {
            action: "offer-made",
            token-id: token-id,
            buyer: tx-sender,
            amount: amount,
            expires-at: expires-at
        })
        
        (ok true)
    )
)

;; Accept offer
(define-public (accept-offer (token-id uint) (buyer principal))
    (let (
        (owner (unwrap! (map-get? nft-owners token-id) ERR-NOT-FOUND))
        (offer (unwrap! (map-get? offers {nft-id: token-id, buyer: buyer}) ERR-NOT-FOUND))
        (amount (get amount offer))
        (expires-at (get expires-at offer))
        (metadata (unwrap! (map-get? nft-metadata token-id) ERR-NOT-FOUND))
        (creator (get creator metadata))
        (royalty-rate (get royalty metadata))
        (marketplace-fee (calculate-marketplace-fee amount))
        (royalty-amount (calculate-royalty amount royalty-rate))
        (seller-amount (- (- amount marketplace-fee) royalty-amount))
    )
        ;; Validate acceptance
        (asserts! (is-eq tx-sender owner) ERR-NOT-OWNER)
        (asserts! (<= (get-current-block) expires-at) ERR-AUCTION-ENDED)
        (asserts! (>= (stx-get-balance buyer) amount) ERR-INSUFFICIENT-FUNDS)
        
        ;; Execute transfer payments
        (try! (stx-transfer? seller-amount buyer tx-sender))
        (try! (stx-transfer? marketplace-fee buyer CONTRACT-OWNER))
        
        ;; Pay royalties to creator (if different from seller)
        (if (and (> royalty-amount u0) (not (is-eq creator tx-sender)))
            (try! (stx-transfer? royalty-amount buyer creator))
            true
        )
        
        ;; Transfer NFT ownership
        (map-set nft-owners token-id buyer)
        
        ;; Remove listing if exists
        (map-delete listings token-id)
        
        ;; Remove offer
        (map-delete offers {nft-id: token-id, buyer: buyer})
        
        ;; Update statistics
        (var-set total-marketplace-revenue 
            (+ (var-get total-marketplace-revenue) marketplace-fee)
        )
        (var-set total-creator-royalties 
            (+ (var-get total-creator-royalties) royalty-amount)
        )
        
        (print {
            action: "offer-accepted",
            token-id: token-id,
            buyer: buyer,
            seller: tx-sender,
            amount: amount
        })
        
        (ok true)
    )
)

;; Cancel offer
(define-public (cancel-offer (token-id uint))
    (let (
        (offer (unwrap! (map-get? offers {nft-id: token-id, buyer: tx-sender}) ERR-NOT-FOUND))
    )
        ;; Remove offer
        (map-delete offers {nft-id: token-id, buyer: tx-sender})
        
        (print {
            action: "offer-cancelled",
            token-id: token-id,
            buyer: tx-sender
        })
        
        (ok true)
    )
)

;; Get offer information
(define-read-only (get-offer (token-id uint) (buyer principal))
    (map-get? offers {nft-id: token-id, buyer: buyer})
)

;; ==================== ADMIN FUNCTIONS ====================

;; Toggle marketplace status (admin only)
(define-public (toggle-marketplace)
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (var-set marketplace-enabled (not (var-get marketplace-enabled)))
        
        (print {
            action: "marketplace-toggled",
            enabled: (var-get marketplace-enabled)
        })
        
        (ok (var-get marketplace-enabled))
    )
)

;; Update contract URI (admin only)
(define-public (set-contract-uri (new-uri (string-ascii 256)))
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (var-set contract-uri new-uri)
        
        (print {
            action: "contract-uri-updated",
            new-uri: new-uri
        })
        
        (ok true)
    )
)

;; Emergency withdrawal (admin only)
(define-public (emergency-withdraw (amount uint))
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-PARAMETERS)
        (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) ERR-INSUFFICIENT-FUNDS)
        
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
        
        (print {
            action: "emergency-withdrawal",
            amount: amount
        })
        
        (ok true)
    )
)
