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
