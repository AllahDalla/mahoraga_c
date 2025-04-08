#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

// define bitboard data type
#define u64 unsigned long long


// FEN dedug positions
#define empty_board "8/8/8/8/8/8/8/8 w - - "
#define start_position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 "
#define tricky_position "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 "
#define killer_position "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq d5 0 1"
#define cmk_position "r2q1rk1/ppp2ppp/2n1bn2/2b1p3/3pP3/3P1NPP/PPP1NPB1/R1BQ1RK1 b - - 0 9 "



/**
 * Global file pointer for logging messages to a text file.
 * Initialized to NULL and will be set to a valid file stream when logging is initialized.
 */
FILE *log_file = NULL;


/**
 * Initializes the log file for writing log messages.
 *
 * Creates a new log file named "mahoraga_log.txt" in write mode.
 * If the file cannot be opened, an error message is printed to stderr.
 * The global log_file pointer is set to the opened file stream.
 */
void init_log() {
    log_file = fopen("mahoraga_log.txt", "w");
    if (log_file == NULL) {
        fprintf(stderr, "Failed to open log file\n");
    }
}

/**
 * Closes the log file if it is currently open.
 *
 * This function safely closes the global log file, ensuring that any buffered
 * log messages are written and system resources are properly released.
 */
void close_log() {
    if (log_file != NULL) {
        fclose(log_file);
    }
}

/**
 * Logs a formatted message to the log file.
 *
 * Writes a formatted message to the previously initialized log file using variable argument list.
 * If the log file is not open, no action is taken. Ensures the message is immediately written
 * to the file by flushing the stream after writing.
 *
 * @param format A printf-style format string
 * @param ... Variable arguments corresponding to the format string
 */
void log_message(const char *format, ...) {
    if (log_file != NULL) {
        va_list args;
        va_start(args, format);
        vfprintf(log_file, format, args);
        va_end(args);
        fflush(log_file);  // Ensure it's written immediately
    }
}







/**
 * Retrieves the current system tick count in milliseconds.
 *
 * @return Current system tick count as an integer
 */
int get_time(){
    return GetTickCount();
}

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


// promoted piece
const char promoted_pieces[] ={
    [Q] = 'q',
    [R] = 'r',
    [B] = 'b',
    [N] = 'n',
    [q] = 'q',
    [r] = 'r',
    [b] = 'b',
    [n] = 'n'
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


/**
 * Creates a deep copy of the current chess board state.
 * 
 * This macro copies all critical board state variables, including:
 * - Piece bitboards
 * - Occupancy bitboards
 * - Side to move
 * - En passant square
 * - Castling rights
 * 
 * Allows for temporary board state manipulation without losing the original board configuration.
 */
#define copy_board() \
    u64 copy_piece_bitboards[12];\
    u64 copy_occupancy_bitboards[3];\
    int copy_side, copy_enpassant, copy_castle;\
    memcpy(copy_piece_bitboards, piece_bitboards, 96);\
    memcpy(copy_occupancy_bitboards, occupancy_bitboards, 24);\
    copy_side = side;\
    copy_enpassant = enpassant;\
    copy_castle = castle;\

/**
 * Restores the chess board state from a previously created deep copy.
 * 
 * This macro reverses the effects of copy_board() by restoring:
 * - Piece bitboards
 * - Occupancy bitboards
 * - Side to move
 * - En passant square
 * - Castling rights
 * 
 * Allows recovery of the original board configuration after temporary manipulation.
 */
#define restore_board()\
    memcpy(piece_bitboards, copy_piece_bitboards, 96);\
    memcpy(occupancy_bitboards, copy_occupancy_bitboards, 24);\
    side = copy_side;\
    enpassant = copy_enpassant;\
    castle = copy_castle;\

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


u64 bishop_magic_numbers[64] = {
    0x40040844404084ULL,
    0x2004208a004208ULL,
    0x10190041080202ULL,
    0x108060845042010ULL,
    0x581104180800210ULL,
    0x2112080446200010ULL,
    0x1080820820060210ULL,
    0x3c0808410220200ULL,
    0x4050404440404ULL,
    0x21001420088ULL,
    0x24d0080801082102ULL,
    0x1020a0a020400ULL,
    0x40308200402ULL,
    0x4011002100800ULL,
    0x401484104104005ULL,
    0x801010402020200ULL,
    0x400210c3880100ULL,
    0x404022024108200ULL,
    0x810018200204102ULL,
    0x4002801a02003ULL,
    0x85040820080400ULL,
    0x810102c808880400ULL,
    0xe900410884800ULL,
    0x8002020480840102ULL,
    0x220200865090201ULL,
    0x2010100a02021202ULL,
    0x152048408022401ULL,
    0x20080002081110ULL,
    0x4001001021004000ULL,
    0x800040400a011002ULL,
    0xe4004081011002ULL,
    0x1c004001012080ULL,
    0x8004200962a00220ULL,
    0x8422100208500202ULL,
    0x2000402200300c08ULL,
    0x8646020080080080ULL,
    0x80020a0200100808ULL,
    0x2010004880111000ULL,
    0x623000a080011400ULL,
    0x42008c0340209202ULL,
    0x209188240001000ULL,
    0x400408a884001800ULL,
    0x110400a6080400ULL,
    0x1840060a44020800ULL,
    0x90080104000041ULL,
    0x201011000808101ULL,
    0x1a2208080504f080ULL,
    0x8012020600211212ULL,
    0x500861011240000ULL,
    0x180806108200800ULL,
    0x4000020e01040044ULL,
    0x300000261044000aULL,
    0x802241102020002ULL,
    0x20906061210001ULL,
    0x5a84841004010310ULL,
    0x4010801011c04ULL,
    0xa010109502200ULL,
    0x4a02012000ULL,
    0x500201010098b028ULL,
    0x8040002811040900ULL,
    0x28000010020204ULL,
    0x6000020202d0240ULL,
    0x8918844842082200ULL,
    0x4010011029020020ULL
};
u64 rook_magic_numbers[64] = {
    0x8a80104000800020ULL,
    0x140002000100040ULL,
    0x2801880a0017001ULL,
    0x100081001000420ULL,
    0x200020010080420ULL,
    0x3001c0002010008ULL,
    0x8480008002000100ULL,
    0x2080088004402900ULL,
    0x800098204000ULL,
    0x2024401000200040ULL,
    0x100802000801000ULL,
    0x120800800801000ULL,
    0x208808088000400ULL,
    0x2802200800400ULL,
    0x2200800100020080ULL,
    0x801000060821100ULL,
    0x80044006422000ULL,
    0x100808020004000ULL,
    0x12108a0010204200ULL,
    0x140848010000802ULL,
    0x481828014002800ULL,
    0x8094004002004100ULL,
    0x4010040010010802ULL,
    0x20008806104ULL,
    0x100400080208000ULL,
    0x2040002120081000ULL,
    0x21200680100081ULL,
    0x20100080080080ULL,
    0x2000a00200410ULL,
    0x20080800400ULL,
    0x80088400100102ULL,
    0x80004600042881ULL,
    0x4040008040800020ULL,
    0x440003000200801ULL,
    0x4200011004500ULL,
    0x188020010100100ULL,
    0x14800401802800ULL,
    0x2080040080800200ULL,
    0x124080204001001ULL,
    0x200046502000484ULL,
    0x480400080088020ULL,
    0x1000422010034000ULL,
    0x30200100110040ULL,
    0x100021010009ULL,
    0x2002080100110004ULL,
    0x202008004008002ULL,
    0x20020004010100ULL,
    0x2048440040820001ULL,
    0x101002200408200ULL,
    0x40802000401080ULL,
    0x4008142004410100ULL,
    0x2060820c0120200ULL,
    0x1001004080100ULL,
    0x20c020080040080ULL,
    0x2935610830022400ULL,
    0x44440041009200ULL,
    0x280001040802101ULL,
    0x2100190040002085ULL,
    0x80c0084100102001ULL,
    0x4024081001000421ULL,
    0x20030a0244872ULL,
    0x12001008414402ULL,
    0x2006104900a0804ULL,
    0x1004081002402ULL
};

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


// BINARY ENCODING KEYS
                                                      
/**
 *                                                                  Hexadecimal Representation
 *  0000 0000 0000 0000 0011 1111 - source square -                            0x3F
 *  0000 0000 0000 1111 1100 0000 - target square -                            0xFC0
 *  0000 0000 1111 0000 0000 0000 - piece -                                    0xF000
 *  0000 1111 0000 0000 0000 0000 - promoted piece -                           0xF0000
 *  0001 0000 0000 0000 0000 0000 - capture flag -                             0x100000
 *  0010 0000 0000 0000 0000 0000 - double pawn push flag -                    0x200000
 *  0100 0000 0000 0000 0000 0000 - en passant flag -                          0x400000
 *  1000 0000 0000 0000 0000 0000 - castling flag -                            0x800000
 */

// define macros to encode and decode each piece of information

/**
 * Encodes a chess move into a compact 32-bit integer representation.
 * 
 * @param source The source square (0-63) of the move
 * @param target The target square (0-63) of the move
 * @param piece The moving piece type (0-15)
 * @param promoted_piece The piece type to promote to, if applicable (0-15)
 * @param capture Flag indicating if the move is a capture (0 or 1)
 * @param double_push Flag indicating if it's a double pawn push (0 or 1)
 * @param en_passant Flag indicating if it's an en passant capture (0 or 1)
 * @param castling Flag indicating if it's a castling move (0 or 1)
 * @return A bitwise-packed 32-bit integer representing the complete move
 */
#define encode_move(source, target, piece, promoted_piece, capture, double_push, en_passant, castling) \
    (source) | (target << 6) | (piece << 12) | (promoted_piece << 16) | (capture << 20) | (double_push << 21) | (en_passant << 22) | (castling << 23)

/**
 * Extracts the source square from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The source square (0-63) derived from the least significant 6 bits
 */
#define get_source_square(move) (move & 0x3F)
/**
 * Extracts the target square from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The target square (0-63) derived from bits 6-11 of the move encoding
 */
/**
 * Extracts the target square from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The target square (0-63) derived from bits 6-11 of the move encoding
 */
#define get_target_square(move) ((move & 0xFC0) >> 6)
/**
 * Extracts the piece from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The piece (0-15) derived from bits 12-15 of the move encoding
 */
#define get_piece(move) ((move & 0xF000) >> 12)
/**
 * Extracts the promoted piece from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The promoted piece (0-15) derived from bits 16-19 of the move encoding
 */
#define get_promoted_piece(move) ((move & 0xF0000) >> 16)
/**
 * Extracts the capture flag from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The capture flag (0 or 1) derived from bit 20 of the move encoding
 */
#define get_capture(move) ((move & 0x100000) >> 20)
/**
 * Extracts the double push flag from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The double push flag (0 or 1) derived from bit 21 of the move encoding
 */
#define get_double_push(move) ((move & 0x200000) >> 21)
/**
 * Extracts the en passant flag from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The en passant flag (0 or 1) derived from bit 22 of the move encoding
 */
#define get_enpassant(move) ((move & 0x400000) >> 22)
/**
 * Extracts the castling flag from an encoded move.
 * 
 * @param move The encoded move containing bitwise-packed move information
 * @return The castling flag (0 or 1) derived from bit 23 of the move encoding
 */
#define get_castling(move) ((move & 0x800000) >> 23)

// move struct
/**
 * Represents a collection of chess moves with a fixed-size array and move count.
 * 
 * @param move_list An array to store up to 256 encoded chess moves
 * @param move_count the index  of the next available move slot in the move_list array
 */
typedef struct {
    int move_list[256];
    int move_count;
} moves;


/**
 * Adds a move to the move list and increments the move count.
 * 
 * @param moves_list Pointer to the moves structure to update
 * @param move The encoded chess move to add to the list
 */
static inline void add_move(moves *moves_list, int move){
    moves_list->move_list[moves_list->move_count] = move;
    moves_list->move_count++;
}


// print move
/**
 * Prints the details of an encoded chess move.
 * 
 * @param move The encoded chess move to be printed, containing source square, target square, and potential promoted piece
 * @brief Displays the source and target squares, along with any promoted piece information
 */
char* print_move(int move){
    static char move_str[10];
    int promoted = get_promoted_piece(move);
    
    if (promoted) {
        sprintf(move_str, "%s%s%c", 
            square_to_coordinate[get_source_square(move)], 
            square_to_coordinate[get_target_square(move)], 
            promoted_pieces[promoted]);
    } else {
        sprintf(move_str, "%s%s", 
            square_to_coordinate[get_source_square(move)], 
            square_to_coordinate[get_target_square(move)]);
    }
    
    return move_str;
}



// print move_list

/**
 * Prints detailed information about all moves in a move list.
 * 
 * Iterates through each move in the move list and prints:
 * - Source and target squares
 * - Promoted piece (if applicable)
 * - Piece type
 * - Move flags (capture, double push, en passant, castling)
 * 
 * @param moves_list Pointer to the moves structure containing the list of moves to print
 */
void print_moves_list(moves *moves_list){
    for(int count = 0; count < moves_list->move_count; count++){
        print_move(moves_list->move_list[count]);
        printf("Piece: %s Capture Flag: %d Double-Push Flag: %d En passant Flag: %d Castling Flag: %d\n\n", ascii_pieces[get_piece(moves_list->move_list[count])], get_capture(moves_list->move_list[count]), get_double_push(moves_list->move_list[count]), get_enpassant(moves_list->move_list[count]), get_castling(moves_list->move_list[count]));
    }
    printf("Move Count: %d\n", moves_list->move_count);
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

enum {all_moves, captures_only};

// castling rights updates constants
const int castling_rights[64] = {
    7, 15, 15, 15,  3, 15, 15, 11,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    13, 15, 15, 15, 12, 15, 15, 14
};

// make move
int make_move(int move, int move_flag){
    // check if move is quiet move
    if(move_flag == all_moves){
        // copy board
        copy_board();

        // get move info
        int source_square = get_source_square(move);
        int target_square = get_target_square(move);
        int piece = get_piece(move);
        int promoted_piece = get_promoted_piece(move);
        int capture_flag = get_capture(move);
        int double_push_flag = get_double_push(move);
        int enpassant_flag = get_enpassant(move);
        int castling_flag = get_castling(move);

        // make move
        pop_bit(piece_bitboards[piece], source_square);
        set_bit(piece_bitboards[piece], target_square);


        // handle captures
        if(capture_flag){
            printf("CAPTURE : Move -> %s\n", print_move(move));
            int start_piece, end_piece;
            if(side == white){
                start_piece = p;
                end_piece = k;
            }else{
                start_piece = P;
                end_piece = K;
            }

            // loop over  all pieces and remove captured piece
            for(int piece = start_piece; piece <= end_piece; piece++){
                // check if piece is on target square
                if(get_bit(piece_bitboards[piece], target_square)){
                    // remove piece from bitboard
                    pop_bit(piece_bitboards[piece], target_square);
                    break;
                }
            }

        }

        // handle pawn promotion
        if(promoted_piece){
            // remove piece from source square
            pop_bit(piece_bitboards[piece], target_square);

            // add promoted piece to target square
            set_bit(piece_bitboards[promoted_piece], target_square);
        }

        // handle en passant
        if(enpassant_flag){
            // remove captured pawn from target square
            (side == white) ? pop_bit(piece_bitboards[p], target_square + 8) : pop_bit(piece_bitboards[P], target_square - 8);
        }

        // reset en passant square
        enpassant = no_sq;

        // handle setting en passant square
        if(double_push_flag){
            (side == white) ? (enpassant = target_square + 8) : (enpassant = target_square - 8);
        }

        // handle castling moves
        if(castling_flag){
            switch(target_square){
                // white king side caastling
                case (g1):
                        pop_bit(piece_bitboards[R], h1);
                        set_bit(piece_bitboards[R], f1);
                        break;
                // white queen side castling
                case (c1):
                        pop_bit(piece_bitboards[R], a1);
                        set_bit(piece_bitboards[R], d1);
                        break;
                // black king side castling
                case (g8):
                        pop_bit(piece_bitboards[r], h8);
                        set_bit(piece_bitboards[r], f8);
                        break;
                // black queen side castling
                case (c8):
                        pop_bit(piece_bitboards[r], a8);
                        set_bit(piece_bitboards[r], d8);
                        break;
            }
        } 

        // update castling rights
        castle &= castling_rights[source_square];
        castle &= castling_rights[target_square];

        // update occupancy bitboards
        memset(occupancy_bitboards, 0ULL, 24);

        // loop over pieces
        for(int piece = P; piece <= k; piece++){
            if(piece >= P && piece <= K){
                occupancy_bitboards[white] |= piece_bitboards[piece];
            }else{
                occupancy_bitboards[black] |= piece_bitboards[piece];
            }
        }

        // update both color occupancy bitboard
        occupancy_bitboards[both] = occupancy_bitboards[white] | occupancy_bitboards[black];

        // // check if king square is in check
        side ^= 1; // flip side 

        if(side == white){
            int square_index = get_lsb_index(piece_bitboards[k]); // get index of black king
            if(is_square_attacked(square_index, side)){
                // take back illegal move
                restore_board();
                return 0; // return that move is illegal
            }else{
                return 1; // return that move is legal
            }
        }else{
            int square_index = get_lsb_index(piece_bitboards[K]); // get index of white king
            if(is_square_attacked(square_index, side)){
                // take back illegal move
                restore_board();
                return 0; // return that move is illegal
            }else{
                return 1; // return that move is legal
            }
        }
    }else{
        // check if move is capture
        if(get_capture(move)){
            return make_move(move, all_moves);
        }else{
            return 0;
        }
    }
}



// generate all moves for a given side
static inline void generate_moves(moves *moves_list){
    u64 bitboard, attacks;
    int source_square, target_square;
    moves_list->move_count = 0; // initialize move count

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
                            add_move(moves_list, encode_move(source_square, target_square, piece, Q, 0, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, R, 0, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, B, 0, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, N, 0, 0, 0, 0));

                        }else{
                            // add to move list
                            add_move(moves_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0)); // pawn push
                            if((source_square >= a2 && source_square <= h2) && get_bit(occupancy_bitboards[both], target_square - 8) == 0){
                                // add to move list
                                add_move(moves_list, encode_move(source_square, (target_square - 8), piece, 0, 0, 1, 0, 0)); // double pawn push
                            }
                        }
                    }

                    // pawn capture moves
                    attacks = pawn_attacks[white][source_square] & occupancy_bitboards[black];

                    while(attacks){
                        // get index of target square
                        target_square = get_lsb_index(attacks);
                        if(source_square >= a7 && source_square <= h7){
                            // add to move list - Promotion
                            add_move(moves_list, encode_move(source_square, target_square, piece, Q, 1, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, R, 1, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, B, 1, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, N, 1, 0, 0, 0));

                        }else{
                            // add to move list
                            add_move(moves_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0)); // pawn capture
                        }
                        pop_bit(attacks, target_square);
                    }


                    if(enpassant != no_sq){
                        u64 enpassant_attacks = pawn_attacks[white][source_square] & (1ULL << enpassant);
                        if(enpassant_attacks){
                            int target_enpasssant = get_lsb_index(enpassant_attacks);
                            add_move(moves_list, encode_move(source_square, target_enpasssant, piece, 0, 1, 0, 1, 0)); // pawn capture enpassant

                        }
                    }

                    // remove source square from bitboard
                    pop_bit(bitboard, source_square);
                }
            }

            // castling moves
            if(piece == K){
                // check if White can castle kingside
                if(castle & white_can_castle_kingside){
                    // check if the squares between the king and the rook are empty
                    if((get_bit(occupancy_bitboards[both], f1) == 0) && (get_bit(occupancy_bitboards[both], g1) == 0)){
                        // check if the squares between the king and the rook are not attacked
                        if((is_square_attacked(e1, black) == 0) && (is_square_attacked(f1, black) == 0)){
                            // add to move list
                            add_move(moves_list, encode_move(e1, g1, piece, 0, 0, 0, 0, 1)); // kingside castle
                        }
                    }
                }

                // check if White can castle queenside
                if(castle & white_can_castle_queenside){
                    // check if the squares between the king and the rook are empty (queenside)
                    if((get_bit(occupancy_bitboards[both], d1) == 0) && (get_bit(occupancy_bitboards[both], c1) == 0) && (get_bit(occupancy_bitboards[both], b1) == 0)){
                        // check if the squares between the king and the rook are not attacked (queenside)
                        if((is_square_attacked(e1, black) == 0) && (is_square_attacked(d1, black) == 0)){
                            // add to move list
                            add_move(moves_list, encode_move(e1, c1, piece, 0, 0, 0, 0, 1)); // queenside castle
                        }
                    }
                }

            }



        }else{ // generate moves for black pieces
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
                            add_move(moves_list, encode_move(source_square, target_square, piece, q, 0, 0, 0, 0)); // pawn promotion
                            add_move(moves_list, encode_move(source_square, target_square, piece, r, 0, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, b, 0, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, n, 0, 0, 0, 0));
                        }else{
                            // add to move list
                            add_move(moves_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0)); // pawn push
                            if((source_square >= a7 && source_square <= h7) && get_bit(occupancy_bitboards[both], target_square + 8) == 0){
                                // add to move list
                                add_move(moves_list, encode_move(source_square, (target_square + 8), piece, 0, 0, 1, 0, 0)); // double pawn push
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
                            add_move(moves_list, encode_move(source_square, target_square, piece, q, 1, 0, 0, 0)); // pawn promotion capture
                            add_move(moves_list, encode_move(source_square, target_square, piece, r, 1, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, b, 1, 0, 0, 0));
                            add_move(moves_list, encode_move(source_square, target_square, piece, n, 1, 0, 0, 0));
                        }else{
                            // add to move list
                            add_move(moves_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0)); // pawn capture
                        }
                        pop_bit(attacks, target_square);
                    }


                    if(enpassant != no_sq){
                        u64 enpassant_attacks = pawn_attacks[black][source_square] & (1ULL << enpassant);
                        if(enpassant_attacks){
                            int target_enpasssant = get_lsb_index(enpassant_attacks);
                            add_move(moves_list, encode_move(source_square, target_enpasssant, piece, 0, 1, 0, 1, 0)); // pawn enpassant capture

                        }
                    }

                    // remove source square from bitboard
                    pop_bit(bitboard, source_square);
                }
            }

            // castling moves
            if(piece == k){
                // check if black can castle kingside
                if(castle & black_can_castle_kingside){
                    // check if the squares between the king and the rook are empty
                    if((get_bit(occupancy_bitboards[both], f8) == 0) && (get_bit(occupancy_bitboards[both], g8) == 0)){
                        // check if the squares between the king and the rook are not attacked
                        if((is_square_attacked(e8, white) == 0) && (is_square_attacked(f8, white) == 0)){
                            // add to move list
                            add_move(moves_list, encode_move(e8, g8, piece, 0, 0, 0, 0, 1)); // kingside castle
                        }
                    }
                }

                // check if black can castle queenside
                if(castle & black_can_castle_queenside){
                    // check if the squares between the king and the rook are empty (queenside)
                    if((get_bit(occupancy_bitboards[both], d8) == 0) && (get_bit(occupancy_bitboards[both], c8) == 0) && (get_bit(occupancy_bitboards[both], b8) == 0)){
                        // check if the squares between the king and the rook are not attacked (queenside)
                        if((is_square_attacked(e8, white) == 0) && (is_square_attacked(d8, white) == 0)){
                            // add to move list
                            add_move(moves_list, encode_move(e8, c8, piece, 0, 0, 0, 0, 1)); // queenside castle
                        }
                    }
                }

            }
        }


        // knight moves
        if((side == white) ? (piece == N) : (piece == n)){
            // loop through all squares
            while(bitboard){
                // get source square
                source_square = get_lsb_index(bitboard);

                attacks = knight_attacks[source_square] & (side == white ? ~occupancy_bitboards[white] : ~occupancy_bitboards[black]);
                while(attacks){
                    // get target square
                    target_square = get_lsb_index(attacks);

                    //quiet move
                    if((side == white) ? (get_bit(occupancy_bitboards[black], target_square) == 0) : (get_bit(occupancy_bitboards[white], target_square) == 0)){
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0)); // knight quiet move
                    }else{
                        // capture move
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0)); // knight capture move
                    }


                    // remove target square from bitboard
                    pop_bit(attacks, target_square);
                }

                // remove source square from bitboard
                pop_bit(bitboard, source_square);
            }
        }

        // bishop moves
        if((side == white) ? (piece == B) : (piece == b)){
            // loop through all squares
            while(bitboard){
                // get source square
                source_square = get_lsb_index(bitboard);

                attacks = get_bishop_attacks(source_square, occupancy_bitboards[both]) & (side == white ? ~occupancy_bitboards[white] : ~occupancy_bitboards[black]);
                while(attacks){
                    // get target square
                    target_square = get_lsb_index(attacks);

                    //quiet move
                    if((side == white) ? (get_bit(occupancy_bitboards[black], target_square) == 0) : (get_bit(occupancy_bitboards[white], target_square) == 0)){
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0)); // bishop quiet move
                    }else{
                        // capture move
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0)); // bishop capture move
                    }


                    // remove target square from bitboard
                    pop_bit(attacks, target_square);
                }

                // remove source square from bitboard
                pop_bit(bitboard, source_square);
            }
        }

        // rook moves
        if((side == white) ? (piece == R) : (piece == r)){
            // loop through all squares
            while(bitboard){
                // get source square
                source_square = get_lsb_index(bitboard);

                attacks = get_rook_attacks(source_square, occupancy_bitboards[both]) & ((side == white) ? ~occupancy_bitboards[white] : ~occupancy_bitboards[black]);
                while(attacks){
                    // get target square
                    target_square = get_lsb_index(attacks);

                    //quiet move
                    if((side == white) ? (get_bit(occupancy_bitboards[black], target_square) == 0) : (get_bit(occupancy_bitboards[white], target_square) == 0)){
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0)); // rook quiet move
                    }else{
                        // capture move
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0)); // rook capture move
                    }


                    // remove target square from bitboard
                    pop_bit(attacks, target_square);
                }

                // remove source square from bitboard
                pop_bit(bitboard, source_square);
            }
        }


        // queen moves
        if((side == white) ? (piece == Q) : (piece == q)){
            // loop through all squares
            while(bitboard){
                // get source square
                source_square = get_lsb_index(bitboard);

                attacks = get_queen_attacks(source_square, occupancy_bitboards[both]) & (side == white ? ~occupancy_bitboards[white] : ~occupancy_bitboards[black]);
                while(attacks){
                    // get target square
                    target_square = get_lsb_index(attacks);

                    //quiet move
                    if((side == white) ? (get_bit(occupancy_bitboards[black], target_square) == 0) : (get_bit(occupancy_bitboards[white], target_square) == 0)){
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0)); // queen quiet move
                    }else{
                        // capture move
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0)); // queen capture move
                    }


                    // remove target square from bitboard
                    pop_bit(attacks, target_square);
                }

                // remove source square from bitboard
                pop_bit(bitboard, source_square);
            }
        }

        // king moves
        if((side == white) ? (piece == K) : (piece == k)){
            // loop through all squares
            while(bitboard){
                // get source square
                source_square = get_lsb_index(bitboard);

                attacks = king_attacks[source_square] & (side == white ? ~occupancy_bitboards[white] : ~occupancy_bitboards[black]);
                while(attacks){
                    // get target square
                    target_square = get_lsb_index(attacks);

                    //quiet move
                    if((side == white) ? (get_bit(occupancy_bitboards[black], target_square) == 0) : (get_bit(occupancy_bitboards[white], target_square) == 0)){
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 0, 0, 0, 0)); // king quiet move
                    }else{
                        // capture move
                        add_move(moves_list, encode_move(source_square, target_square, piece, 0, 1, 0, 0, 0)); // king capture move
                    }


                    // remove target square from bitboard
                    pop_bit(attacks, target_square);
                }

                // remove source square from bitboard
                pop_bit(bitboard, source_square);
            }
        }

    }

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

        int relevant_bits = count_bits(attack_mask);

        int occupancy_indicies = (1 << relevant_bits);

        for(int index = 0; index < occupancy_indicies; index++){
            if(isBishop){
                u64 occupancy = set_occupancy(index, relevant_bits, attack_mask);
                int magic_index = ((occupancy * bishop_magic_numbers[square]) >> (64 - bishop_relevant_bits[square]));
                bishop_attacks[square][magic_index] = generate_bishop_attacks(square, occupancy);
            
            }else{
                u64 occupancy = set_occupancy(index, relevant_bits, attack_mask);
                int magic_index = ((occupancy * rook_magic_numbers[square]) >> (64 - rook_relevant_bits[square]));
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
    // init_magics();
    init_slider_attacks(bishop);
    init_slider_attacks(rook);
}


/* 
    ********************************************
    *
    *               PERFT
    * 
    * 
    ******************************************** 
*/

/** 
 * Global counter to track the number of nodes explored during performance testing.
 * 
 * Used in perft (performance testing) to count the total number of legal moves 
 * generated and explored during a recursive move tree traversal.
 */
long nodes;

/**
 * Recursively explores the move tree for performance testing.
 * 
 * This function is an internal helper for perft (performance testing) that:
 * - Generates all possible moves at the current board state
 * - Recursively explores each move to the specified depth
 * - Increments the global nodes counter when reaching the base depth
 * 
 * @param depth The remaining depth to explore in the move tree
 */
static inline void perf_driver(int depth){
    // base condition
    if(depth == 0){
        nodes++;
        return;
    }

    moves move_list[1];
    generate_moves(move_list);

    for (int count = 0; count < move_list->move_count; count++)
    {
        // init move
        int move = move_list->move_list[count];
        
        // preserve board state
        copy_board();
        if(!make_move(move, all_moves)){
            // illegal move
            continue;
        }
        
        perf_driver(depth - 1);
        // take back
        restore_board();
    }

}

/**
 * Performs a performance test (perft) to count the number of legal moves at a given depth.
 * 
 * This function generates all possible moves, recursively explores the move tree,
 * and prints detailed performance statistics including:
 * - Number of nodes (legal moves) explored
 * - Time elapsed
 * - Depth of search
 * 
 * @param depth The maximum depth to explore in the move tree
 */
void perft(int depth){
    printf("Performance Test : \n\n");

    int start_time = get_time();

    moves move_list[1];
    generate_moves(move_list);

    for (int count = 0; count < move_list->move_count; count++)
    {
        // init move
        int move = move_list->move_list[count];
        
        // preserve board state
        copy_board();
        if(!make_move(move, all_moves)){
            // illegal move
            continue;
        }

        long cummalative_nodes = nodes;

        perf_driver(depth - 1);
        
        long old_nodes = nodes - cummalative_nodes;
        // take back
        print_move(move);
        printf("Nodes [inside]: %ld\n", old_nodes);
        
        restore_board();

    }

    printf("\n\nTime Elapsed: %d ms\n", get_time() - start_time);
    printf("Nodes: %ld\n", nodes);
    printf("Depth: %d\n", depth);
    printf("--------------------------------------\n");

}

/* 
    ********************************************
    *
    *               UCI
    * 
    * 
    ******************************************** 
*/

/**
 * Parses a move string and validates it against the current board's legal moves.
 * 
 * Converts a UCI-style move string (e.g., "e2e4" or "e7e8q") into a valid chess move.
 * Handles both standard moves and promotion moves by checking source, target squares,
 * and optional promotion piece.
 * 
 * @param move_str A string representing the move in UCI notation
 * @return The corresponding move if valid, or 0 if the move is illegal
 */
int parse_move(char* move_str){
    // create moves list and generate moves
    moves moves_list[1];
    generate_moves(moves_list);

    // parse move string
    int source_square = (move_str[0] - 'a') + (8 - (move_str[1] - '0')) * 8;
    int target_square = (move_str[2] - 'a') + (8 - (move_str[3] - '0')) * 8;
    
    // loop over all moves in move list
    for(int count = 0; count <= moves_list->move_count; count ++){
        // get move
        int move = moves_list->move_list[count];

        // check if parsed moves are in move list
        if(source_square == get_source_square(move) && target_square == get_target_square(move)){
            // get promoted piece if any
            int promoted_piece = get_promoted_piece(move);
            // check for promoted piece
            if(promoted_piece){
                // check if move string contains the proper promoted piece matched with move in move list
                if((promoted_piece == Q || promoted_piece == q) && move_str[4] == 'q'){
                    return move;
                }else if((promoted_piece == R || promoted_piece == r) && move_str[4] == 'r'){
                    return move;
                }else if((promoted_piece == B || promoted_piece == b) && move_str[4] == 'b'){
                    return move;
                }else if((promoted_piece == N || promoted_piece == n) && move_str[4] == 'n'){
                    return move;
                }
                // return false if not matches were found
                continue;
            }
            // return legal move
            return move;
        }
    }
    // return move is illegal
    return 0;
}


// parse position

/**
 * Parses a UCI position command to set up the chess board.
 * 
 * Handles two types of position commands:
 * 1. "startpos" to set the initial chess starting position
 * 2. "fen [FEN_STRING]" to set a custom board position
 * 
 * If moves are specified after the position, the function applies 
 * those moves sequentially to the board.
 * 
 * @param command The full UCI position command string
 */
void parse_position(char *command){
    // point to command
    command += 9;
    // init pointer
    char *current_char = command;

    if(strncmp(command, "startpos", 8) == 0){
        // start position
        parse_fen(start_position);
    }else{
        // parse fen 
        current_char = strstr(command, "fen");
        if(current_char == NULL){
            // parse fen
            parse_fen(start_position);
        }else{
            current_char += 4;
            parse_fen(current_char);
        }
    }

    // parse moves
    current_char = strstr(command, "moves");

    if(current_char != NULL){
        // parse moves
        current_char += 6;
        
        while(*current_char){
            int move = parse_move(current_char);

            if(move == 0){
                break;
            }

            make_move(move, all_moves);

            while(*current_char && *current_char != ' '){
                current_char++;
            }

            // go to next move
            current_char++;
        }
    }

}

/* 
    ********************************************
    *
    *               EVALUATION
    * 
    * 
    ******************************************** 
*/

const int material_score[12] = {
    100, // white pawn
    300, // white knight
    350, // white bishop
    500, // white rook
    1000, // white queen
    10000, // white king
    -100, // black pawn
    -300, // black knight
    -350, // black bishop
    -500, // black rook
    -1000,// black queen
    -10000 // black king
};

// pawn positional score
const int pawn_score[64] = 
{
    90,  90,  90,  90,  90,  90,  90,  90,
    30,  30,  30,  40,  40,  30,  30,  30,
    20,  20,  20,  30,  30,  30,  20,  20,
    10,  10,  10,  20,  20,  10,  10,  10,
     5,   5,  10,  20,  20,   5,   5,   5,
     0,   0,   0,   5,   5,   0,   0,   0,
     0,   0,   0, -10, -10,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0
};

// knight positional score
const int knight_score[64] = 
{
    -5,   0,   0,   0,   0,   0,   0,  -5,
    -5,   0,   0,  10,  10,   0,   0,  -5,
    -5,   5,  20,  20,  20,  20,   5,  -5,
    -5,  10,  20,  30,  30,  20,  10,  -5,
    -5,  10,  20,  30,  30,  20,  10,  -5,
    -5,   5,  20,  10,  10,  20,   5,  -5,
    -5,   0,   0,   0,   0,   0,   0,  -5,
    -5, -10,   0,   0,   0,   0, -10,  -5
};

// bishop positional score
const int bishop_score[64] = 
{
     0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,  10,  10,   0,   0,   0,
     0,   0,  10,  20,  20,  10,   0,   0,
     0,   0,  10,  20,  20,  10,   0,   0,
     0,  10,   0,   0,   0,   0,  10,   0,
     0,  30,   0,   0,   0,   0,  30,   0,
     0,   0, -10,   0,   0, -10,   0,   0

};

// rook positional score
const int rook_score[64] =
{
    50,  50,  50,  50,  50,  50,  50,  50,
    50,  50,  50,  50,  50,  50,  50,  50,
     0,   0,  10,  20,  20,  10,   0,   0,
     0,   0,  10,  20,  20,  10,   0,   0,
     0,   0,  10,  20,  20,  10,   0,   0,
     0,   0,  10,  20,  20,  10,   0,   0,
     0,   0,  10,  20,  20,  10,   0,   0,
     0,   0,   0,  20,  20,   0,   0,   0

};

// king positional score
const int king_score[64] = 
{
     0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   5,   5,   5,   5,   0,   0,
     0,   5,   5,  10,  10,   5,   5,   0,
     0,   5,  10,  20,  20,  10,   5,   0,
     0,   5,  10,  20,  20,  10,   5,   0,
     0,   0,   5,  10,  10,   5,   0,   0,
     0,   5,   5,  -5,  -5,   0,   5,   0,
     0,   0,   5,   0, -15,   0,  10,   0
};

// mirror positional score tables for opposite side
const int mirror_score[128] =
{
	a1, b1, c1, d1, e1, f1, g1, h1,
	a2, b2, c2, d2, e2, f2, g2, h2,
	a3, b3, c3, d3, e3, f3, g3, h3,
	a4, b4, c4, d4, e4, f4, g4, h4,
	a5, b5, c5, d5, e5, f5, g5, h5,
	a6, b6, c6, d6, e6, f6, g6, h6,
	a7, b7, c7, d7, e7, f7, g7, h7,
	a8, b8, c8, d8, e8, f8, g8, h8
};



static inline int evaluate(){
    int score = 0;
    u64 bitboard;
    int piece, square;
    // loop over piece bitboards
    for(int board_piece = P; board_piece <= k; board_piece++){
        bitboard = piece_bitboards[board_piece];
        // get piece score
        while(bitboard){
            piece = board_piece;
            square = get_lsb_index(bitboard);
            // score piece
            score += material_score[piece];

            // add positional score
            switch(piece){
                // evalue white pieces
                case P:
                    score += pawn_score[square];
                    break;
                case N:
                    score += knight_score[square];
                    break;
                case B:
                    score += bishop_score[square];
                    break;
                case R:
                    score += rook_score[square];
                    break;
                case K:
                    score += king_score[square];
                    break;
                // evalue black pieces
                case p:
                    score -= pawn_score[mirror_score[square]];
                    break;
                case n:
                    score -= knight_score[mirror_score[square]];
                    break;
                case b:
                    score -= bishop_score[mirror_score[square]];
                    break;
                case r:
                    score -= rook_score[mirror_score[square]];
                    break;
                case k:
                    score -= king_score[mirror_score[square]];
                    break;
            }

            // remove piece from bitboard
            pop_bit(bitboard, square);

        }

    }

    return (side == white) ? score : -score;
}




/* 
    ********************************************
    *
    *               SEARCH
    * 
    * 
    ******************************************** 
*/

// half move counter
int ply;
// best move
int best_move;


static inline int q_search(int alpha, int beta){

    int eval = evaluate();

    // fail high
    if(eval >= beta){
        return beta;
    }

    // fail low
    if(eval > alpha){
        alpha = eval;
    }

    moves move_list[1];
    generate_moves(move_list);
    for(int count = 0; count < move_list->move_count; count++){
        // init move
        int move = move_list->move_list[count];

        // preserve board state
        copy_board();

        // increment ply
        ply++;

        
        if(make_move(move, captures_only) == 0){
            // illegal move
            ply--;
            continue;
        }
        printf("Side %s; Q_Move -> %s\n", (side == white) ? "White": "Black", print_move(move));

        // increment nodes
        nodes++;
        // increment legal moves

        // negamax
        int score = -q_search(-beta, -alpha);
        // print_chessboard();

        // restore board
        restore_board();
        // decrement ply
        ply--;


        // fail high
        if(score >= beta){
            return beta;
        }

        // fail low
        if(score > alpha){
            alpha = score;
        }
    }

    return alpha;
}

/**
 * Implements the Negamax algorithm for chess move evaluation.
 * 
 * Recursively searches game tree to find the best move within given depth,
 * using alpha-beta pruning to optimize search efficiency. Evaluates moves 
 * by generating all possible moves, exploring each move's potential score, 
 * and tracking the best move at the root level.
 * 
 * @param alpha Lower bound of the search window
 * @param beta Upper bound of the search window
 * @param depth Remaining search depth
 * @return The best evaluation score for the current position
 */
static inline int negamax(int alpha, int beta, int depth){
    // base condition
    if(depth == 0){
        return q_search(alpha, beta);
    }

    int in_check = is_square_attacked(get_lsb_index((side == white) ? piece_bitboards[K]:piece_bitboards[k]), (side ^ 1));
    int legal_moves = 0;
    int best;
    int old_alpha = alpha;
    moves move_list[1];
    generate_moves(move_list);
    for(int count = 0; count < move_list->move_count; count++){
        // init move
        int move = move_list->move_list[count];

        // preserve board state
        copy_board();

        // increment ply
        ply++;

        if(make_move(move, all_moves) == 0){
            // illegal move
            ply--;
            continue;
        }

        printf("Side %s; Move -> %s\n", (side == white) ? "White": "Black", print_move(move));
        // increment nodes
        nodes++;
        // increment legal moves
        legal_moves++;

        // negamax
        int score = -negamax(-beta, -alpha, depth - 1);
        // print_chessboard();

        // restore board
        restore_board();
        // decrement ply
        ply--;


        // fail high
        if(score >= beta){
            return beta;
        }

        // fail low
        if(score > alpha){
            alpha = score;
            if(ply == 0){
                best = move;
            }
        }
    }
    // check for checkmate or stalemate
    if(legal_moves == 0){
        if(in_check){
            return -990995 + ply; // checkmate
        }
        else{ // stalemate
            return 0;
        }
    }

    if(old_alpha != alpha){
        best_move = best;
    }

    return alpha;
}

/**
 * Defines an infinite value used in chess engine search algorithms.
 * 
 * Used as a large constant for initializing search bounds in negamax 
 * and alpha-beta pruning algorithms, representing a theoretically 
 * unreachable score value.
 */
#define INF 1000000

/**
 * Initiates the chess move search process.
 * 
 * Performs a negamax search to a specified depth and prints the best move found.
 * Uses infinite bounds and the predefined search depth to evaluate possible moves.
 * 
 * @param depth The maximum search depth for move evaluation
 */
void search(int depth){
    int score = negamax(-INF, INF, depth);
    if(best_move){
        printf("info score cp %d depth %d nodes %ld\n", score, depth, nodes);
        printf("bestmove %s", print_move(best_move));
        printf("\n");
        log_message("bestmove %s\n", print_move(best_move));
        log_message("Score: %d\n", score);
    }
}

/**
 * Parses the search depth from a 'go' command.
 * 
 * Extracts the depth value from the command string. If no depth is specified,
 * defaults to a search depth of 6.
 * 
 * @param command The input command string containing search parameters
 */
void parse_go(char *command){
    // parse depth
    int depth = 0;
    char *current_char = strstr(command, "depth");
    if(current_char != NULL){
        current_char += 6;
        depth = atoi(current_char);
    }else{
        // default depth
        depth = 6;
    }
    // search
    search(depth);
}

/**
 * Name of the chess engine.
 * 
 * Defines the identifier used when reporting the engine's name
 * during UCI (Universal Chess Interface) communication.
 */
char *engine_name = "Mahoraga";
/**
 * Name of the chess engine's author.
 * 
 * Defines the identifier used when reporting the engine's author
 * during UCI (Universal Chess Interface) communication.
 */
char *engine_author = "Allah Dalla";

void uci_loop(){

    init_log();
    log_message("Initializing engine\n");

    // clear buffers
    setbuf(stdin, NULL);
    setbuf(stdout, NULL);

    // input buffer
    char input_buffer[3000];

    // print engine info
    printf("id name %s\n", engine_name);
    printf("id author %s\n", engine_author);
    printf("uciok\n");
    log_message("Sent initial UCI identification\n");

    // loop
    while(1){
        // clear buffer
        memset(input_buffer, 0, sizeof(input_buffer));
        // fflush(stdin);

        if(!fgets(input_buffer, 3000, stdin)){
            continue;
        }

        log_message("Lucas : %s\n", input_buffer);

        if(input_buffer[0] == '\n'){
            continue;
        }

        // parse command
        if(strncmp(input_buffer, "isready", 7) == 0){
            // ready
            printf("readyok\n");
            log_message("Mahoraga : readyok\n");
            continue;
        }
        
        if(strncmp(input_buffer, "position", 8) == 0){
            // parse position
            parse_position(input_buffer);
            log_message("Mahoraga : Parsed position command\n");
            continue;
        }
        // parse new game
        if(strncmp(input_buffer, "ucinewgame", 10) == 0){
            // new game
            parse_position("position startpos");
            log_message("Mahoraga : Parsed new game command\n");
            continue;
        }

        // parse go
        if(strncmp(input_buffer, "go", 2) == 0){
            // parse go
            parse_go(input_buffer);
            log_message("Mahoraga : Parsed go command\n");
            continue;
        }

        // parse quit
        if(strncmp(input_buffer, "quit", 4) == 0 || strncmp(input_buffer, "stop", 4) == 0){
            // quit
            log_message("Mahoraga : Quitting\n");
            break;
        }

        if(strncmp(input_buffer, "uci", 3) == 0){
            // print engine info
            printf("id name %s\n", engine_name);
            printf("id author %s\n", engine_author);
            printf("uciok\n");
            log_message("Sent initial UCI identification\n");
            continue;
        }
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


/**
 * Main entry point for the chess engine program.
 * 
 * Demonstrates various chess engine functionalities including:
 * - Parsing FEN positions
 * - Setting search depth
 * - Printing the chessboard
 * 
 * @note This is a test/development main function with commented-out experimental code
 * @return Exit status of the program
 */
int main(){
    
    initialize_engine();   
    
    uci_loop();

    return 0;
}