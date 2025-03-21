#include <stdio.h>
#include <string.h>

// define bitboard data type
#define u64 unsigned long long


// FEN dedug positions
#define empty_board "8/8/8/8/8/8/8/8 w - - "
#define start_position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 "
#define tricky_position "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 "
#define killer_position "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq d5 0 1"
#define cmk_position "r2q1rk1/ppp2ppp/2n1bn2/2b1p3/3pP3/3P1NPP/PPP1NPB1/R1BQ1RK1 b - - 0 9 "


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
    no_sq
};

enum {white, black, both};
enum{rook, bishop};

// castling rights flags definition

/**
 * 0001 - 1: white can castle kingside
 * 0010 - 2: white can castle queenside
 * 0100 - 4: black can castle kingside
 * 1000 - 8: black can castle queenside
 * 1111 - 15: both sides can castle
 * 1001 - 9: white king side castle & black king queenside castle
 */

 //enum for castling rights
 enum{
    white_can_castle_kingside = 1,
    white_can_castle_queenside = 2,
    black_can_castle_kingside = 4,
    black_can_castle_queenside = 8,
};

enum {
    P, N, B, R, Q, K, p, n, b, r, q, k
};

// piece names
const char * ascii_pieces[12] = {
    "P", "N", "B", "R", "Q", "K", "p", "n", "b", "r", "q", "k"
};

// Unicode chess pieces
const wchar_t* chess_pieces_unicode[12];

// piece bitboards
u64 piece_bitboards[12];

// occupancy bitboards
u64 occupancy_bitboards[3];

// side to move
int side;

// enpassant square
int enpassant = no_sq;

// castling rights
int castle;





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
#define get_bit(bitboard, square) ((bitboard) & (1ULL << (square)))
#define set_bit(bitboard, square) ((bitboard) |= (1ULL << (square)))
#define pop_bit(bitboard, square) (get_bit(bitboard, square) ? (bitboard) ^= (1ULL << (square)) : 0)

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
    *               HELPER FUNCTIONS
    * 
    * 
    ******************************************** 
*/

void print_chessboard(){
    // loop over ranks
    printf("\n");
    for(int rank = 0; rank < 8; rank++){
        // loop over files
        for(int file = 0; file < 8; file++){
            // get square index
            int square = rank * 8 + file;
            if(!file){
                printf("%d ", 8 - rank);
            }

            // get piece at square
            int piece = -1;

            for(int count = 0; count < 12; count++){
                if(get_bit(piece_bitboards[count], square)){
                    piece = count;
                }
            }

            printf("%s ", ((piece == -1) ? "." : ascii_pieces[piece]));
            
        }
        printf("\n");
    }

    printf("\n  a b c d e f g h\n");

    printf("\n");

    printf("side:             %s\n", ((side == white) ? "white" : "black"));
    printf("enpassant:        %s\n", ((enpassant == no_sq) ? "none" : square_to_coordinate[enpassant]));
    printf("castle:           %c%c%c%c\n", ((castle & white_can_castle_kingside) ? 'K': '-'), ((castle & white_can_castle_queenside) ? 'Q': '-'), ((castle & black_can_castle_kingside) ? 'k': '-'), ((castle & black_can_castle_queenside) ? 'q': '-'));

}


// parse fen string

/**
 * Converts a character representation of a chess piece to its corresponding piece enum.
 *
 * @param c The character representing a chess piece (uppercase for white, lowercase for black)
 * @return The piece enum value, or -1 if the character is not a valid chess piece
 */
int char_to_piece(int c) {
    switch(c) {
        case 'P': return P;
        case 'N': return N;
        case 'B': return B;
        case 'R': return R;
        case 'Q': return Q;
        case 'K': return K;
        case 'p': return p;
        case 'n': return n;
        case 'b': return b;
        case 'r': return r;
        case 'q': return q;
        case 'k': return k;
        default: return -1; // Not a valid piece
    }
}

/**
 * Parses a Forsyth-Edwards Notation (FEN) string and initializes the chess board state.
 *
 * This function populates piece bitboards, sets the active side, determines castling rights,
 * and sets the en passant square based on the provided FEN string.
 *
 * @param fen A null-terminated string representing the chess board state in FEN format
 */
void parse_fen(char *fen){


    // clear bitboards and occupancy bitboards and initialize side to white, enpassant to no_sq, and castle to 0
    memset(piece_bitboards, 0ULL, sizeof(piece_bitboards));
    memset(occupancy_bitboards, 0ULL, sizeof(occupancy_bitboards));
    side = white;
    enpassant = no_sq;
    castle = 0;

    int index = 0;
    int space = 1;

    // set board state from fen string
    for(int rank = 0; rank < 8; rank++){
        for(int file = 0; file < 8; file++){
            if(fen[index] == '/'){
                index++;
            }
            // number of spaces
            if(fen[index] >= '1' && fen[index] <= '8'){
                int temp = fen[index] - '0';
                if(temp == 8){
                    index++;
                    break;
                }
                if(space < temp){
                    space++;
                    continue;
                }else{
                    index++;
                    space = 1;
                    continue;
                }
            }

            

            int square = rank * 8 + file;
            char c = fen[index++];
            int piece = (int)char_to_piece(c);
            set_bit(piece_bitboards[piece], square);
        }
    }

    // set side, enpassant, and castle
    for(int i = 0; fen[index] != '\0'; i++){
        if(fen[index] == ' '){
            index++;
            continue;
        }

        if(fen[index] == 'w'){
            side = white;
            index++;
            continue;
        }
        
        if(fen[index] == 'b'){
            side = black;
            index++;
            continue;
        }

        // white king can castle kingside
        if(fen[index] == 'K'){
            castle |= white_can_castle_kingside;
            index++;
            continue;
        }
        // white king can castle queenside
        if(fen[index] == 'Q'){
            castle |= white_can_castle_queenside;
            index++;
            continue;
        }
        // black king can castle kingside
        if(fen[index] == 'k'){
            castle |= black_can_castle_kingside;
            index++;
            continue;
        }
        // black king can castle queenside
        if(fen[index] == 'q'){
            castle |= black_can_castle_queenside;
            index++;
            continue;
        }

        if(fen[index] >= 'a' && fen[index] <= 'h'){
            //  enpassant square
            int rank = (int)(8 - (fen[index + 1] - '0'));
            int file = (int)(fen[index] - 'a'); 
            enpassant = rank * 8 + file;
        }

        index++;
    }

    for(int piece_color = P; piece_color <= k; piece_color++){
        if(piece_color >= P && piece_color <= K){
            occupancy_bitboards[white] |= piece_bitboards[piece_color];
        }else{
            occupancy_bitboards[black] |= piece_bitboards[piece_color];
        }
    }

    occupancy_bitboards[both] |= occupancy_bitboards[white] | occupancy_bitboards[black];

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

u64 rook_masks[64];
u64 bishop_masks[64];

u64 pawn_attacks[2][64];
u64 knight_attacks[64];
u64 king_attacks[64];
u64 rook_attacks[64][4096]; // [square][occupancy]
u64 bishop_attacks[64][512]; // [square][occupancy]

/**
 * Defines the number of relevant bits for bishop magic bitboard generation on each square.
 *
 * These values are used to calculate the size of the occupancy variations for bishop attacks
 * on different board squares, which is crucial for efficient magic bitboard move generation.
 *
 * @note The values vary based on the square's position on the chessboard, with edge squares
 * having fewer relevant bits than central squares.
 */
const int bishop_relevant_bits[64] ={
    6,  5,  5,  5,  5,  5,  5,  6, 
    5,  5,  5,  5,  5,  5,  5,  5, 
    5,  5,  7,  7,  7,  7,  5,  5, 
    5,  5,  7,  9,  9,  7,  5,  5, 
    5,  5,  7,  9,  9,  7,  5,  5, 
    5,  5,  7,  7,  7,  7,  5,  5, 
    5,  5,  5,  5,  5,  5,  5,  5, 
    6,  5,  5,  5,  5,  5,  5,  6, 
};

/**
 * Defines the number of relevant bits for rook magic bitboard generation on each square.
 *
 * These values are used to calculate the size of the occupancy variations for rook attacks
 * on different board squares, which is crucial for efficient magic bitboard move generation.
 *
 * @note The values vary based on the square's position on the chessboard, with edge squares
 * having more relevant bits than central squares.
 */
const int rook_relevant_bits[64] = {
    12,  11,  11,  11,  11,  11,  11,  12, 
    11,  10,  10,  10,  10,  10,  10,  11, 
    11,  10,  10,  10,  10,  10,  10,  11, 
    11,  10,  10,  10,  10,  10,  10,  11, 
    11,  10,  10,  10,  10,  10,  10,  11, 
    11,  10,  10,  10,  10,  10,  10,  11, 
    11,  10,  10,  10,  10,  10,  10,  11,
    12,  11,  11,  11,  11,  11,  11,  12,
};


u64 bishop_magic_numbers[64];
u64 rook_magic_numbers[64];

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
 * Generates attack bitboard for a bishop at the given square, considering blocking pieces.
 *
 * @param square The chess board square (0-63) where the bishop is located
 * @param block A bitboard representing blocking pieces
 * @return A bitboard representing all squares the bishop can attack from the given square
 */
u64 get_bishop_attacks(int square, u64 block){
    block &= bishop_masks[square];
    block *= bishop_magic_numbers[square];
    block >>= 64 - bishop_relevant_bits[square];

    return bishop_attacks[square][block];
}

/**
 * Generates attack bitboard for a rook at the given square, considering blocking pieces.
 *
 * @param square The chess board square (0-63) where the rook is located
 * @param block A bitboard representing blocking pieces
 * @return A bitboard representing all squares the rook can attack from the given square
 */
u64 get_rook_attacks(int square, u64 block){
    block &= rook_masks[square];
    block *= rook_magic_numbers[square];
    block >>= 64 - rook_relevant_bits[square];

    return rook_attacks[square][block];
}


/**
 * Generates attack bitboard for a queen at the given square, considering blocking pieces.
 *
 * @param square The chess board square (0-63) where the queen is located
 * @param block A bitboard representing blocking pieces
 * @return A bitboard representing all squares the queen can attack from the given square
 */
u64 get_queen_attacks(int square, u64 block){
    return get_bishop_attacks(square, block) | get_rook_attacks(square, block);
}

/* 
    ********************************************
    *
    *               MOVE GENERATION
    * 
    * 
    ******************************************** 
*/


// generate all moves for a given side
static inline void generate_moves(){
    u64 bitboard, attacks;
    int source_square, target_square;

    for(int piece = P; piece <= k; piece++){
        bitboard = piece_bitboards[piece];
        // generate moves for white pieces
        if(side == white){
            // generate moves for pawns
            if(piece == P){
                while(bitboard){
                    // get index of source square
                    source_square = get_lsb_index(bitboard);
                    // get index of target square
                    target_square = source_square - 8;

                    // PAWN QUIET MOVES
                    // check if target square is empty
                    if(target_square >= a8 && get_bit(occupancy_bitboards[both], target_square) == 0){
                        // pawn promotion
                        if(source_square >= a7 && source_square <= h7){
                            // add to move list
                            printf("pawn promotion: %s-%s q\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn promotion: %s-%s r\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn promotion: %s-%s b\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn promotion: %s-%s n\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                        }else{
                            // add to move list
                            printf("pawn push: %s-%s\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            if((source_square >= a2 && source_square <= h2) && get_bit(occupancy_bitboards[both], target_square - 8) == 0){
                                // add to move list
                                printf("double pawn push: %s-%s\n", square_to_coordinate[source_square], square_to_coordinate[target_square - 8]);
                            }
                        }
                    }

                    // pawn capture moves
                    attacks = pawn_attacks[white][source_square] & occupancy_bitboards[black];

                    while(attacks){
                        // get index of target square
                        target_square = get_lsb_index(attacks);
                        if(source_square >= a7 && source_square <= h7){
                            // add to move list
                            printf("pawn capture promotion: %s-%s q\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn capture promotion: %s-%s r\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn capture promotion: %s-%s b\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn capture promotion: %s-%s n\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                        }else{
                            // add to move list
                            printf("pawn capture: %s-%s\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                        }
                        pop_bit(attacks, target_square);
                    }


                    if(enpassant != no_sq){
                        u64 enpassant_attacks = pawn_attacks[white][source_square] & (1ULL << enpassant);
                        if(enpassant_attacks){
                            int target_enpasssant = get_lsb_index(enpassant_attacks);
                            printf("pawn enpassant: %s-%s\n", square_to_coordinate[source_square], square_to_coordinate[target_enpasssant]);

                        }
                    }

                    // remove source square from bitboard
                    pop_bit(bitboard, source_square);
                }
            }
        }else{
            if(piece == p){
                while(bitboard){
                    // get index of source square
                    source_square = get_lsb_index(bitboard);
                    // get index of target square
                    target_square = source_square + 8;

                    // check if target square is empty
                    if(target_square <= h1 && get_bit(occupancy_bitboards[both], target_square) == 0){
                        // pawn promotion
                        if(source_square >= a2 && source_square <= h2){
                            // add to move list
                            printf("pawn promotion: %s-%s q\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn promotion: %s-%s r\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn promotion: %s-%s b\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn promotion: %s-%s n\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                        }else{
                            // add to move list
                            printf("pawn push: %s-%s\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            if((source_square >= a7 && source_square <= h7) && get_bit(occupancy_bitboards[both], target_square + 8) == 0){
                                // add to move list
                                printf("double pawn push: %s-%s\n", square_to_coordinate[source_square], square_to_coordinate[target_square + 8]);
                            }
                        }
                    }

                    // pawn capture moves
                    attacks = pawn_attacks[black][source_square] & occupancy_bitboards[white];

                    while(attacks){
                        // get index of target square
                        target_square = get_lsb_index(attacks);
                        if(source_square >= a2 && source_square <= h2){
                            // add to move list
                            printf("pawn capture promotion: %s-%s q\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn capture promotion: %s-%s r\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn capture promotion: %s-%s b\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                            printf("pawn capture promotion: %s-%s n\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                        }else{
                            // add to move list
                            printf("pawn capture: %s-%s\n", square_to_coordinate[source_square], square_to_coordinate[target_square]);
                        }
                        pop_bit(attacks, target_square);
                    }


                    if(enpassant != no_sq){
                        u64 enpassant_attacks = pawn_attacks[black][source_square] & (1ULL << enpassant);
                        if(enpassant_attacks){
                            int target_enpasssant = get_lsb_index(enpassant_attacks);
                            printf("pawn enpassant: %s-%s\n", square_to_coordinate[source_square], square_to_coordinate[target_enpasssant]);

                        }
                    }

                    // remove source square from bitboard
                    pop_bit(bitboard, source_square);
                }
            }
        }
    }

}



/**
 * Determines if a given square is under attack by any opponent's piece.
 *
 * Checks for potential attacks from pawns, knights, bishops, rooks, queens, and kings
 * based on the current board state and the specified side.
 *
 * @param square The chess board square (0-63) to check for attacks
 * @param side The color of the side being checked for potential attacks against
 * @return 1 if the square is attacked, 0 otherwise
 * 
 * eg usage: for checking if a king is in check.
 */
static inline int is_square_attacked(int square, int side){

    // pawns
    if((side == white) ? (pawn_attacks[black][square] & piece_bitboards[P]) : (pawn_attacks[white][square] & piece_bitboards[p])) return 1;

    // knights
    if((side == white) ? (knight_attacks[square] & piece_bitboards[N]) : (knight_attacks[square] & piece_bitboards[n])) return 1;

    // bishops
    if((side == white) ? (get_bishop_attacks(square, occupancy_bitboards[both]) & piece_bitboards[B]) : (get_bishop_attacks(square, occupancy_bitboards[both]) & piece_bitboards[b])) return 1;

    // rooks
    if((side == white) ? (get_rook_attacks(square, occupancy_bitboards[both]) & piece_bitboards[R]) : (get_rook_attacks(square, occupancy_bitboards[both]) & piece_bitboards[r])) return 1;

    // queens
    if((side == white) ? (get_queen_attacks(square, occupancy_bitboards[both]) & piece_bitboards[Q]) : (get_queen_attacks(square, occupancy_bitboards[both]) & piece_bitboards[q])) return 1;

    // kings
    if((side == white) ? (king_attacks[square] & piece_bitboards[K]) : (king_attacks[square] & piece_bitboards[k])) return 1;

    return 0;
}


/* 
    ********************************************
    *
    *               MAGIC NUMBERS 
    *                    &
    *               LOOKUP TABLES
    * 
    * 
    ******************************************** 
*/

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


/**
 * Generates an occupancy bitboard for a given attack mask and index.
 * 
 * @param index The index used to determine bit placement in the occupancy bitboard.
 * @param bit_count The number of bits to set in the occupancy bitboard.
 * @param attack_mask A bitboard representing possible attack squares.
 * @return A 64-bit unsigned integer representing the generated occupancy configuration.
 */
u64 set_occupancy(int index, int bit_count, u64 attack_mask){
    u64 occupancy = 0ULL;

    for(int count = 0; count < bit_count; count++){
        int square = get_lsb_index(attack_mask);
        pop_bit(attack_mask, square);
        if(index & (1 << count)){
            occupancy |= (1ULL << square);
        }
    }

    return occupancy;
}

// create random number generator for cross-platform

unsigned int seed = 1804289383;

/**
 * Generates a pseudo-random 32-bit unsigned integer using a simple bitwise XOR shift algorithm.
 * 
 * @return A pseudo-random 32-bit unsigned integer generated by bitwise manipulation of the seed value.
 */
unsigned int random(){
    unsigned int number  = seed;
    number ^= number << 13;
    number ^= number >> 17;
    number ^= number << 5;

    seed = number;
    return number;
};

/**
 * Generates a 64-bit random number using bitwise operations.
 * 
 * @return A 64-bit unsigned random number generated by combining four 16-bit random values.
 */
u64 random_64(){
    u64 num1 = (u64)(random()) & 0xFFFF; 
    u64 num2 = (u64)(random()) & 0xFFFF;
    u64 num3 = (u64)(random()) & 0xFFFF;
    u64 num4 = (u64)(random()) & 0xFFFF;

    return num1 | (num2 << 16) | (num3 << 32) | (num4 << 48);

}

// generate magic number candidate

/**
 * Generates a candidate magic number for bitboard attack generation.
 * 
 * @return A 64-bit unsigned integer representing a potential magic number used in bitboard chess programming.
 */
u64 get_magic_candidate(){
    return random_64() & random_64() & random_64();
}


/**
 * Finds a magic number for bishop or rook attack generation in bitboard chess programming.
 * 
 * @param square The chess board square (0-63) for which to generate the magic number
 * @param relevant_bits The number of relevant bits for attack mask generation
 * @param isBishop Flag to determine whether to generate magic number for bishop (1) or rook (0)
 * @return A 64-bit magic number used for efficient attack lookup in bitboard representation
 */
u64 find_magic_number(int square, int relevant_bits, int isBishop){
    u64 occupancy[4096];
    u64 attacks[4096];
    int used_attacks[4096];
    u64 attack_mask = isBishop ? mask_bishop_attacks(square) : mask_rook_attacks(square);
    int occupancy_indicies = 1 << relevant_bits;

    // generate occupancy indicies
    for(int index = 0; index < occupancy_indicies; index++){
        occupancy[index] = set_occupancy(index, relevant_bits, attack_mask);
        attacks[index] = isBishop ? generate_bishop_attacks(square, occupancy[index]) : generate_rook_attacks(square, occupancy[index]);
    }

    // test magic number
    for(int random_count = 0; random_count < 100000000; random_count++){
        
        u64 magic = get_magic_candidate();
        // skip unusable magic numbers
        if(count_bits((attack_mask * magic) & 0xFF00000000000000) < 6) continue;
        
        memset(used_attacks, 0ULL, sizeof(used_attacks));

        // init flags to test magic number
        int index, fail;

        for(index = 0, fail = 0; !fail && index < occupancy_indicies; index++){
            int magic_index = (int)((occupancy[index] * magic) >> (64 - relevant_bits));
            // check if magic index has been used
            if(used_attacks[magic_index] == 0ULL){
                // init magic index
                used_attacks[magic_index] = attacks[index];
            }else if(used_attacks[magic_index] != attacks[index]){
                fail = 1;
            }
        }
        // if magic number is valid, return it
        if(!fail){
            return magic;
        }
    }
    printf("Magic number not found\n");
    return 0ULL;
}



/**
 * Initializes magic numbers for rook and bishop attack tables.
 * 
 * Generates and stores pre-computed magic numbers for efficient bitboard
 * attack lookups for both rook and bishop pieces across all 64 board squares.
 */
void init_magics(){
    for(int square = 0; square < 64; square++){
       rook_magic_numbers[square] = find_magic_number(square, rook_relevant_bits[square], rook);
    }

    for(int square = 0; square < 64; square++){
       bishop_magic_numbers[square] = find_magic_number(square, bishop_relevant_bits[square], bishop);
    }
}

/**
 * Initializes pre-computed attack tables for slider pieces (bishops and rooks).
 * 
 * Generates attack masks and populates attack lookup tables using magic bitboard technique
 * for both bishops and rooks across all 64 board squares. This function creates efficient
 * attack pattern lookups by using magic numbers and occupancy configurations.
 * 
 * @param isBishop Flag to determine whether to generate attacks for bishops (1) or rooks (0)
 */
void init_slider_attacks(int isBishop){
    for(int square = 0; square < 64; square++){
        bishop_masks[square] = mask_bishop_attacks(square);
        rook_masks[square] = mask_rook_attacks(square);

        u64 attack_mask = isBishop ? bishop_masks[square] : rook_masks[square];
        // int relevant_bits = count_bits(attack_mask);

        int relevant_bits = isBishop ? bishop_relevant_bits[square] : rook_relevant_bits[square];

        int occupancy_indicies = 1 << relevant_bits;

        for(int index = 0; index < occupancy_indicies; index++){
            if(isBishop){
                u64 occupancy = set_occupancy(index, relevant_bits, attack_mask);
                int magic_index = (int)((occupancy * bishop_magic_numbers[square]) >> (64 - bishop_relevant_bits[square]));
                bishop_attacks[square][magic_index] = generate_bishop_attacks(square, occupancy);
            
            }else{
                u64 occupancy = set_occupancy(index, relevant_bits, attack_mask);
                int magic_index = (int)((occupancy * rook_magic_numbers[square]) >> (64 - rook_relevant_bits[square]));
                rook_attacks[square][magic_index] = generate_rook_attacks(square, occupancy);
            
            }
        }


    }
}


/**
 * Initializes the chess engine by setting up attack tables and magic bitboards.
 * 
 * This function prepares the engine for chess move generation by:
 * - Initializing leaper piece attack patterns
 * - Generating magic numbers for slider pieces (bishops and rooks)
 * - Precomputing attack tables for bishops and rooks
 */
void initialize_engine(){
    init_leaper_attacks();
    init_magics();
    init_slider_attacks(bishop);
    init_slider_attacks(rook);
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
    
    initialize_engine();   

    parse_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPpP/R3K2R b KQkq a3 0 1 ");
    print_chessboard();
    generate_moves();

    return 0;
}