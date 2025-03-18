#include <stdio.h>

// define bitboard data type
#define u64 unsigned long long

// board squares
enum{

    a8, b8, c8, d8, e8, f8, g8, h8,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a1, b1, c1, d1, e1, f1, g1, h1,
};

enum {white, black};

// all squares
/**
 * Mapping of board square indices to their corresponding algebraic chess notation coordinates.
 * Allows conversion from zero-based square indices to standard chess board coordinate strings.
 * Ordered from top-left (a8) to bottom-right (h1) of the chessboard.
 */
const char *square_to_coordinate[] = {
    "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8",
    "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
    "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
    "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
    "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
    "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
    "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
};


/* 
    ********************************************
    *
    *               BIT MANIPULATION
    * 
    * 
    ******************************************** 
*/

// bit macros
#define get_bit(bitboard, square) (bitboard & (1ULL << square))
#define set_bit(bitboard, square) (bitboard |= (1ULL << square))
#define pop_bit(bitboard, square) (get_bit(bitboard, square) ? bitboard ^= (1ULL << square) : 0)

// count bits on bitboard
/**
 * Counts the number of set bits (1s) in a 64-bit bitboard.
 * Uses Brian Kernighan's algorithm for efficient bit counting.
 *
 * @param bitboard The 64-bit unsigned integer to count bits in
 * @return Number of set bits in the bitboard
 */
static inline int count_bits(u64 bitboard){
    int count = 0;
    while(bitboard){
        count++;
        bitboard &= bitboard - 1;
    }
    return count;
}

/**
 * Finds the index of the least significant bit (LSB) in a bitboard.
 * Uses bit manipulation to efficiently determine the position of the rightmost set bit.
 *
 * @param bitboard The 64-bit unsigned integer to find the LSB index in
 * @return The zero-based index of the least significant bit, or -1 if no bits are set
 */
static inline int get_lsb_index(u64 bitboard){
    if(bitboard){
        return count_bits((bitboard & -bitboard) - 1);
    }
    return -1;
}

/**
 * Prints a visual representation of a bitboard, displaying each bit's position
 * on a chessboard grid with rank and file labels.
 *
 * @param bitboard The 64-bit unsigned integer representing the bitboard to print
 */
void print_bitboard(u64 bitboard){
    // loop over ranks
    printf("\n");
    for(int rank = 0; rank < 8; rank++){
        // loop over files
        for(int file = 0; file < 8; file++){
            // get square index
            int square = rank * 8 + file;
            // get bit at square
            u64 bit = get_bit(bitboard, square) ? 1 : 0;
            // print bit

            if(!file){
                printf("%d ", 8 - rank);
            }

            printf(" %llu", bit);
        }
        printf("\n");
    }

    printf("\n   a b c d e f g h\n");

    // unsigned long long decimal of bitboard
    printf("\n  bitboard: %llu decimal\n", bitboard);
}

/* 
    ********************************************
    *
    *               ATTACKS
    * 
    * 
    ******************************************** 
*/
// constant for Not A File
const u64 not_a_file = 18374403900871474942ULL;
// constant for Not H File
const u64 not_h_file = 9187201950435737471ULL;
// constant for not HG File
const u64 not_hg_file = 4557430888798830399ULL;
// constant for not AB File
const u64 not_ab_file = 18229723555195321596ULL;

u64 pawn_attacks[2][64];
u64 knight_attacks[64];
u64 king_attacks[64];
u64 bishop_masks[64];
u64 rook_masks[64];

/**
 * Generates a bitboard representing pawn attack squares for a given side and square.
 *
 * @param side Color of the pawn (0 for white, 1 for black)
 * @param square The starting square of the pawn
 * @return A bitboard representing the squares the pawn can attack
 */
u64 mask_pawn_attacks(int side, int square){
    u64 attacks = 0ULL;
    u64 bitboard = 0ULL; 

    set_bit(bitboard, square);

    // for white pawns
    if(!side){
        if((bitboard >> 7) & not_a_file){
            attacks |= (bitboard >> 7);
        }

        if((bitboard >> 9) & not_h_file){
            attacks |= (bitboard >> 9);
        }
    }else{
        // for black pawns
        if((bitboard << 7) & not_h_file){
            attacks |= (bitboard << 7);
        }

        if((bitboard << 9) & not_a_file){
            attacks |= (bitboard << 9);
        }
    }

    return attacks;
}

/**
 * Generates a bitboard representing knight attack squares for a given square.
 *
 * @param square The starting square of the knight
 * @return A bitboard representing the squares the knight can attack
 */
u64 mask_knight_attacks(int square){
    u64 attacks = 0ULL;
    u64 bitboard = 0ULL;
    set_bit(bitboard, square);

    if((bitboard >> 17) & not_h_file) attacks |= (bitboard >> 17);
    if((bitboard >> 15) & not_a_file) attacks |= (bitboard >> 15);
    if((bitboard >> 10) & not_hg_file) attacks |= (bitboard >> 10);
    if((bitboard >> 6) & not_ab_file) attacks |= (bitboard >> 6);
    if((bitboard << 17) & not_a_file) attacks |= (bitboard << 17);
    if((bitboard << 15) & not_h_file) attacks |= (bitboard << 15);
    if((bitboard << 10) & not_ab_file) attacks |= (bitboard << 10);
    if((bitboard << 6) & not_hg_file) attacks |= (bitboard << 6);

    return attacks;

}

/**
 * Generates a bitboard representing king attack squares for a given square.
 *
 * @param square The starting square of the king
 * @return A bitboard representing the squares the king can attack
 */
u64 mask_king_attacks(int square){
    u64 attacks = 0ULL;
    u64 bitboard = 0ULL;
    set_bit(bitboard, square);

    if((bitboard >> 9) & not_h_file) attacks |= (bitboard >> 9);
    if(bitboard >> 8) attacks |= (bitboard >> 8);
    if((bitboard >> 7) & not_a_file) attacks |= (bitboard >> 7);
    if((bitboard >> 1) & not_h_file) attacks |= (bitboard >> 1);

    if((bitboard << 9) & not_a_file) attacks |= (bitboard << 9);
    if(bitboard << 8) attacks |= (bitboard << 8);
    if((bitboard << 7) & not_h_file) attacks |= (bitboard << 7);
    if((bitboard << 1) & not_a_file) attacks |= (bitboard << 1);

    return attacks;
}

// bishop occupancy mask
u64 mask_bishop_attacks(int square){
    u64 attacks = 0ULL;
    u64 bitboard = 0ULL;
    set_bit(bitboard, square);

    int rank, file;
    int target_rank = square / 8;
    int target_file = square % 8;

    for(rank = target_rank + 1, file = target_file + 1; rank <= 6 && file <= 6; rank++, file++){
        attacks |= (1ULL << (rank * 8 + file));
    }

    for(rank = target_rank - 1, file = target_file + 1; rank >= 1 && file <= 6; rank--, file++){
        attacks |= (1ULL << (rank * 8 + file));
    }

    for(rank = target_rank + 1, file = target_file - 1; rank <= 6 && file >= 1; rank++, file--){
        attacks |= (1ULL << (rank * 8 + file));
    }

    for(rank = target_rank - 1, file = target_file - 1; rank >= 1 && file >= 1; rank--, file--){
        attacks |= (1ULL << (rank * 8 + file));
    }

    return attacks;
}

// rook occupancy bits
u64 mask_rook_attacks(int square){
    u64 attacks = 0ULL;
    u64 bitboard = 0ULL;
    set_bit(bitboard, square);

    int rank, file;
    int target_rank = square / 8;
    int target_file = square % 8;

    for(rank = target_rank + 1; rank <= 6; rank++){
        attacks |= (1ULL << (rank * 8 + target_file));
    }

    for(rank = target_rank - 1; rank >= 1; rank--){
        attacks |= (1ULL << (rank * 8 + target_file));
    }

    for(file = target_file + 1; file <= 6; file++){
        attacks |= (1ULL << (target_rank * 8 + file));
    }

    for(file = target_file - 1; file >= 1; file--){
        attacks |= (1ULL << (target_rank * 8 + file));
    }

    return attacks;

}



/**
 * Generates attack bitboard for a bishop at the given square, considering blocking pieces.
 *
 * @param square The chess board square (0-63) where the bishop is located
 * @param block A bitboard representing blocking pieces
 * @return A bitboard representing all squares the bishop can attack from the given square
 */
u64 generate_bishop_attacks(int square, u64 block){
    u64 attacks = 0ULL;
    u64 bitboard = 0ULL;
    set_bit(bitboard, square);

    int rank, file;
    int target_rank = square / 8;
    int target_file = square % 8;

    for(rank = target_rank + 1, file = target_file + 1; rank <= 7 && file <= 7; rank++, file++){
        attacks |= (1ULL << (rank * 8 + file));
        if((1ULL << (rank * 8 + file)) & block) break;
    }

    for(rank = target_rank - 1, file = target_file + 1; rank >= 0 && file <= 7; rank--, file++){
        attacks |= (1ULL << (rank * 8 + file));
        if((1ULL << (rank * 8 + file)) & block) break;
    }

    for(rank = target_rank + 1, file = target_file - 1; rank <= 7 && file >= 0; rank++, file--){
        attacks |= (1ULL << (rank * 8 + file));
        if((1ULL << (rank * 8 + file)) & block) break;
    }

    for(rank = target_rank - 1, file = target_file - 1; rank >= 0 && file >= 0; rank--, file--){
        attacks |= (1ULL << (rank * 8 + file));
        if((1ULL << (rank * 8 + file)) & block) break;
    }

    return attacks;
}


/**
 * Generates attack bitboard for a rook at the given square, considering blocking pieces.
 *
 * @param square The chess board square (0-63) where the rook is located
 * @param block A bitboard representing blocking pieces
 * @return A bitboard representing all squares the rook can attack from the given square
 */
u64 generate_rook_attacks(int square, u64 block){
    u64 attacks = 0ULL;
    u64 bitboard = 0ULL;
    set_bit(bitboard, square);

    int rank, file;
    int target_rank = square / 8;
    int target_file = square % 8;

    for(rank = target_rank + 1; rank <= 7; rank++){
        attacks |= (1ULL << (rank * 8 + target_file));
        if((1ULL << (rank * 8 + target_file)) & block) break;
    }

    for(rank = target_rank - 1; rank >= 0; rank--){
        attacks |= (1ULL << (rank * 8 + target_file));
        if((1ULL << (rank * 8 + target_file)) & block) break;
    }

    for(file = target_file + 1; file <= 7; file++){
        attacks |= (1ULL << (target_rank * 8 + file));
        if((1ULL << (target_rank * 8 + file)) & block) break;
    }

    for(file = target_file - 1; file >= 0; file--){
        attacks |= (1ULL << (target_rank * 8 + file));
        if((1ULL << (target_rank * 8 + file)) & block) break;
    }

    return attacks;

}

/**
 * Initializes pre-computed attack bitboards for leaping pieces (pawns, knights, and kings).
 * 
 * Populates global attack lookup tables for each square on the chess board,
 * generating attack patterns for white and black pawns, knights, and kings.
 */
void init_leaper_attacks(){
    for(int square = 0; square < 64; square++){
        // pawn attacks
        pawn_attacks[white][square] = mask_pawn_attacks(white, square);
        pawn_attacks[black][square] = mask_pawn_attacks(black, square);

        // knight attacks
        knight_attacks[square] = mask_knight_attacks(square);

        // king attacks
        king_attacks[square] = mask_king_attacks(square);
    }
}


/* 
    ********************************************
    *
    *               MAIN DRIVER
    * 
    * 
    ******************************************** 
*/

int main(){
    init_leaper_attacks(); 

    u64 bitboard = 0ULL;
    set_bit(bitboard, d6);
    set_bit(bitboard, d2);
    set_bit(bitboard, g4);
    set_bit(bitboard, b4);
    print_bitboard(bitboard);
    printf("index -> %d  coordinate: %s", get_lsb_index(bitboard), square_to_coordinate[get_lsb_index(bitboard)]);


    return 0;
}