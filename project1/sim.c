//Joel Miller
//CPSC 3300
//10/27/2023


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* since the simulation deals only with one-word instructions and */
/*   one-word operands, we represent memory as an array of words  */

#define MEM_SIZE_IN_WORDS 256*1024

int mem[MEM_SIZE_IN_WORDS];

/* processor state, simulation state, and instruction fields    */

int reg[32]   = {0}, /* general register set, r0 is always 0    */
    xip       = 0,   /* execute instruction pointer             */
    fip       = 0,   /* fetch instruction pointer               */
    halt_flag = 0,   /* set by halt instruction                 */
    verbose   = 0,   /* governs amount of detail in output      */
    ir,              /* 32-bit instruction register             */
    op1,             /* 6-bit primary opcode in bits 31 to 27   */
    d,               /* 5-bit destination register identifier   */
    s1,              /* 5-bit source 1 register identifier      */
    s2,              /* 5-bit source 2 register identifier      */
    imm16,           /* 16-bit immediate field                  */
    scaled,          /* scaled addressing mode bit 9            */
    eff_addr;        /* 32-bit effective address                */
    int cc_bit = 0,  /* Condition Code Bit                      */
    lowOffset,       /* 11-bit low offset                       */
    highOffset,      /* 5-bit high offset                       */   
    offset16;        /* total offset                            */



/* dynamic execution statistics */

int inst_fetches = 0,
    memory_reads = 0,
    memory_writes = 0,
    branches = 0,
    taken_branches = 0;

/* load memory from stdin */

#define INPUT_WORD_LIMIT 255
void get_mem(){
  int w, count = 0;

  if(verbose > 1){
    printf("reading words in hex from stdin:\n");
  }
  while(scanf( "%x", &w) != EOF ){
    if(verbose > 1) printf( "  0%08x\n", w);
    if(count > INPUT_WORD_LIMIT){
      printf("too many words loaded\n");
      exit(0);
    }
    mem[count] = w;
    count++;
  }
  if(verbose > 1) printf("\n");
}

void read_mem(int eff_addr, int reg_index){
  int word_addr = eff_addr >> 2;
  if( verbose ) printf( "  read access at address %x\n", eff_addr );
  assert( ( word_addr >= 0 ) && ( word_addr < MEM_SIZE_IN_WORDS ) );
  reg[ reg_index ] = mem[ word_addr ];
  memory_reads++;
}

void write_mem( int eff_addr, int reg_index ){
  int word_addr = eff_addr >> 2;
  if( verbose ) printf( "  write access at address %x\n", eff_addr );
  assert( ( word_addr >= 0 ) && ( word_addr < MEM_SIZE_IN_WORDS ) );
  mem[ word_addr ] = reg[ reg_index ];
  memory_writes++;
}

/* extract fields - switch statements are in main loop */
void decode(){
  op1            = ( ir >> 26 ) & 0x3f;
  d              = ( ir >> 16 ) & 0x1f;
  s1             = ( ir >> 11 ) & 0x1f;
  s2             = ( ir >> 21 ) & 0x1f;
  imm16          =   ir         & 0xffff;
  lowOffset      =   ir       & 0x7ff;
  highOffset     = ( ir >> 16) & 0x1f;
  offset16       = ((highOffset << 11) | lowOffset) & 0xffff;
}





// 0x0 - Halt
void halt(){
  if(verbose){
    printf( "halt\n" );
  }

  halt_flag = 1;
}

// 0x4 - Load Integer (General Format)
void ld_l() {
  if (verbose) {
    // Print a message indicating the operation and registers involved
    printf("ld.l  r%x(r%x),r%x\n", s1, s2, d);
  }

  // Calculate the effective address by adding the values in s2 and s1 registers
  eff_addr = reg[s2] + reg[s1];
  // Read the memory at the calculated effective address and store it in register d
  read_mem(eff_addr, d);
}


// 0x5 - Load Integer (Immediate Format)
void ld_li() {
  if (verbose) {
    // Print a message indicating the operation, immediate value, and registers involved
    printf("ld.l %x(r%x),r%x\n", imm16, s2, d);
  }

  // Check the sign bit of imm16 and sign-extend if necessary
  if (imm16 & (1 << 15)) {
    imm16 |= 0xFFFF0000;
  }

  // Calculate the effective address by adding the value in s2 register and the immediate value
  // Subtracting 1 from eff_addr seems to be an unusual step; consider verifying the logic.
  eff_addr = reg[s2] + imm16 - 1;

  // Read the memory at the calculated effective address and store it in register d
  read_mem(eff_addr, d);
}


// 0x7 - Store Integer (Store Format)
void st_l() {
  if (verbose) {
    // Print a message indicating the operation and registers involved
    printf("st.l  r%x,%x(r%x)\n", s1, offset16, s2);
  }

  // Left shift and right shift offset16 to preserve its sign extension
  offset16 = offset16 << 16;
  offset16 = offset16 >> 16;

  // Calculate the effective address by adding the values in s2 and offset16
  // Subtracting 1 from eff_addr seems to be an unusual step; consider verifying the logic.
  eff_addr = reg[s2] + offset16 - 1;

  // Write the value from register s1 to the memory location at eff_addr
  write_mem(eff_addr, s1);
}


// 0x14 - Branch if Not Equal (BTE Format)
void btne() {
  // Print a message indicating the operation and registers involved
  printf("btne  r%x,r%x,%x", s1, s2, offset16);

  // Left shift and right shift offset16 to preserve its sign extension
  offset16 = offset16 << 16;
  offset16 = offset16 >> 16;

  // Check if registers s1 and s2 are not equal
  if (reg[s1] != reg[s2]) {
    // Calculate the new instruction pointer (fip) based on the offset16 and xip
    fip = xip + (offset16 << 2) + 4;

    // Increment the count of taken branches
    taken_branches++;
  }

  if (verbose) {
    // Print a message about the offset16 value in decimal if it's out of the range [-9, 9]
    if ((offset16 < 0) || (offset16 > 9)) {
      printf(" (= decimal %d)\n", offset16);
    } else {
      printf("\n");
    }
  }

  // Increment the total branches count
  branches++;
}


// 0x15 - Branch if Not Equal with Immediate (BTEI Format)
void btnei() {
  // Print a message indicating the operation, immediate value, and registers involved
  printf("btnei %x,r%x,%x", s1, s2, offset16);

  // Left shift and right shift offset16 to preserve its sign extension
  offset16 = offset16 << 16;
  offset16 = offset16 >> 16;

  // Check if the immediate value s1 is not equal to the value in register s2
  if (s1 != reg[s2]) {
    // Calculate the new instruction pointer (fip) based on the offset16 and xip
    fip = xip + (offset16 << 2) + 4;

    // Increment the count of taken branches
    taken_branches++;
  }

  if (verbose) {
    // Print a message about the offset16 value in decimal if it's out of the range [-9, 9]
    if ((offset16 < 0) || (offset16 > 9)) {
      printf(" (= decimal %d)\n", offset16);
    } else {
      printf("\n");
    }
  }

  // Increment the total branches count
  branches++;
}

// 0x16 - Branch if Equal (BTE Format)
void bte() {
   // Print a message indicating the operation and registers involved
   printf("bte   r%x,r%x,%x", s1, s2, offset16);

   // Check if registers s1 and s2 are equal
   if (reg[s1] == reg[s2]) {
      // Left shift and right shift offset16 to preserve its sign extension
      offset16 = offset16 << 16; 
      offset16 = offset16 >> 16;

      // Calculate the new instruction pointer (fip) based on the offset16 and xip
      fip = xip + (offset16 << 2) + 4;

      // Increment the count of taken branches
      taken_branches++;
   }

   if (verbose) {
      // Print a message about the offset16 value in decimal if it's out of the range [-9, 9]
      if ((offset16 < 0) || (offset16 > 9)) {
         printf(" (= decimal %d)\n", offset16);
      } else {
         printf("\n");
      }
   }

   // Increment the total branches count
   branches++;
}


// 0x17 - Branch if Equal with Immediate (BTEI Format)
void btei() {
  // Print a message indicating the operation, immediate value, and registers involved
  printf("btei  %x,r%x,%x", s1, s2, offset16);

  // Check if the immediate value s1 is equal to the value in register s2
  if (s1 == reg[s2]) {
    // Left shift and right shift offset16 to preserve its sign extension
    offset16 = offset16 << 16;
    offset16 = offset16 >> 16;

    // Calculate the new instruction pointer (fip) based on the offset16 and xip
    fip = xip + (offset16 << 2) + 4;

    // Increment the count of taken branches
    taken_branches++;
  }

  if (verbose) {
    // Print a message about the offset16 value in decimal if it's out of the range [-9, 9]
    if ((offset16 < 0) || (offset16 > 9)) {
      printf(" (= decimal %d)\n", offset16);
    } else {
      printf("\n");
    }
  }

  // Increment the total branches count
  branches++;
}


// 0x1a - Unconditional Branch (CTRL Format)
void br() {
  // Extract the lower 26 bits of the instruction to determine the new instruction pointer
  int bitwise = ir & 0x03ffffff;

  if (verbose) {
    // Print a message indicating the operation and the extracted value
    printf("br    %x", bitwise);
  }

  // Left shift and right shift the extracted value to preserve its sign extension
  bitwise = bitwise << 6;
  bitwise = bitwise >> 6;

  // Calculate the new instruction pointer (fip) based on the shifted value and xip
  fip = xip + (bitwise << 2) + 4;

  if (verbose) {
    // Print a message about the shifted value in decimal if it's out of the range [-9, 9]
    if ((bitwise < 0) || (bitwise > 9)) {
      printf(" (= decimal %d)\n", bitwise);
    } else {
      printf("\n");
    }
  }

  // Increment the total branches and taken branches counts
  branches++;
  taken_branches++;
}

// 0x1c - Branch on Condition Code Set (CTRL Format)
void bc() {
  // Extract the lower 26 bits of the instruction to determine the new instruction pointer
  int bitwise = ir & 0x03ffffff;

  if (verbose) {
    // Print a message indicating the operation and the extracted value
    printf("bc    %x", bitwise);
  }

  // Check if the condition code bit (cc_bit) is set (equal to 1)
  if (cc_bit == 1) {
    // Left shift and right shift the extracted value to preserve its sign extension
    bitwise = bitwise << 6;
    bitwise = bitwise >> 6;

    // Calculate the new instruction pointer (fip) based on the shifted value and xip
    fip = xip + (bitwise << 2) + 4;

    // Increment the count of taken branches
    taken_branches++;
  }

  if (verbose) {
    // Print a message about the shifted value in decimal if it's out of the range [-9, 9]
    if ((bitwise < 0) || (bitwise > 9)) {
      printf(" (= decimal %d)\n", bitwise);
    } else {
      printf("\n");
    }
  }

  // Increment the total branches count
  branches++;
}


// 0x1e - Branch on Condition Code Clear (CTRL Format)
void bnc() {
  // Extract the lower 26 bits of the instruction to determine the new instruction pointer
  int bitwise = ir & 0x03ffffff;

  if (verbose) {
    // Print a message indicating the operation and the extracted value
    printf("bnc   %x", bitwise);
  }

  // Check if the condition code bit (cc_bit) is clear (equal to 0)
  if (cc_bit == 0) {
    // Left shift and right shift the extracted value to preserve its sign extension
    bitwise = bitwise << 6;
    bitwise = bitwise >> 6;

    // Calculate the new instruction pointer (fip) based on the shifted value and xip
    fip = xip + (bitwise << 2) + 4;

    // Increment the count of taken branches
    taken_branches++;
  }

  if (verbose) {
    // Print a message about the shifted value in decimal if it's out of the range [-9, 9]
    if ((bitwise < 0) || (bitwise > 9)) {
      printf(" (= decimal %d)\n", bitwise);
    } else {
      printf("\n");
    }
  }

  // Increment the total branches count
  branches++;
}


// 0x24 - Add Signed (General Format)
void adds() {
  if (verbose) {
    // Print a message indicating the operation and registers involved
    printf("adds  r%x,r%x,r%x\n", s1, s2, d);
  }

  // Retrieve the values from registers s1 and s2
  int src1 = reg[s1];
  int src2 = reg[s2];

  // Perform the addition and store the result in register d
  reg[d] = src1 + src2;

  // Set the condition code (cc_bit) based on the addition result
  // If src1 is less than -src2, set cc_bit to 1; otherwise, set it to 0
  cc_bit = (src1 < -src2) ? 1 : 0;
}



// 0x25 - Add Signed, immediate format
void addi() {
  if (verbose) {
    // Print a message indicating the operation, immediate value, and registers involved
    printf("adds  %x,r%x,r%x\n", imm16, s2, d);
  }

  // Retrieve the value from register s2
  int src2 = reg[s2];

  // Left shift and right shift imm16 to preserve its sign extension
  imm16 = imm16 << 16;
  imm16 = imm16 >> 16;

  // Perform the addition of the immediate value and the value in register s2, then store the result in register d
  reg[d] = imm16 + src2;

  // Set the condition code (cc_bit) based on the addition result
  // If src2 is less than -imm16, set cc_bit to 1; otherwise, set it to 0
  cc_bit = (src2 < -imm16) ? 1 : 0;
}


// 0x26 - Subtract Signed, General Format
void subs() {
  if (verbose) {
    // Print a message indicating the operation and registers involved
    printf("subs  r%x,r%x,r%x\n", s1, s2, d);
  }

  // Retrieve the values from registers s1 and s2
  int src1 = reg[s1];
  int src2 = reg[s2];

  // Perform the subtraction and store the result in register d
  reg[d] = reg[s1] - reg[s2];

  // Set the condition code (cc_bit) based on the subtraction result
  // If src2 is greater than src1, set cc_bit to 1; otherwise, set it to 0
  cc_bit = (src2 > src1) ? 1 : 0;
}


// 0x27 - Subtract Signed, Immediate Format
void subi() {
    if (verbose) {
        // Print a message indicating the operation, immediate value, and registers involved
        printf("subs  %x,r%x,r%x\n", imm16, s2, d);
    }

    // Retrieve the value from register s2
    int src2 = reg[s2];

    // Perform the subtraction of the immediate value and the value in register s2, then store the result in register d
    reg[d] = imm16 - reg[s2];

    // Set the condition code (cc_bit) based on the subtraction result
    // If src2 is greater than imm16, set cc_bit to 1; otherwise, set it to 0
    cc_bit = (src2 > imm16) ? 1 : 0;
}


// 0x28 - Shift Left (General Format)
void shls() {
  if (verbose) {
    // Print a message indicating the operation and registers involved
    printf("shl   r%x,r%x,r%x\n", s1, s2, d);
  }

  // Perform a logical left shift of the value in register s1 by the number of bits specified in register s2,
  // and store the result in register d
  reg[d] = reg[s1] << reg[s2];
}


// 0x29 - Shift Left with Immediate (Immediate Format)
void shli() {
  if (verbose) {
    // Print a message indicating the operation, immediate value, and registers involved
    printf("shli  %x,r%x,r%x\n", imm16, s2, d);
  }

  // Perform a logical left shift of the value in register s2 by the number of bits specified in imm16,
  // and store the result in register d
  reg[d] = reg[s2] << imm16;
}


// 0x2a - Shift Right Logical (General Format)
void shrs() {
  if (verbose) {
    // Print a message indicating the operation and registers involved
    printf("shr   r%x,r%x,r%x\n", s1, s2, d);
  }

  // Convert the values from registers s1 and s2 to unsigned integers
  unsigned src1 = reg[s1];
  unsigned src2 = (unsigned)reg[s2];

  // Perform a logical right shift of the value in register s2 by the number of bits specified in register s1,
  // and store the result in register d
  reg[d] = src2 >> src1;
}


// 0x2e - Shift Right Arithmetic (General Format)
void shras() {
  if (verbose) {
    // Print a message indicating the operation and registers involved
    printf("shra  r%x,r%x,r%x\n", s1, s2, d);
  }

  int src2 = reg[s2];
  int total = reg[d];

  // Perform an arithmetic right shift of the value in register s2 by the number of bits specified in register d,
  // and store the result in register d
  reg[d] = src2 >> total;

  // Check if src2 is negative, and if so, perform sign extension by OR-ing with 1s in the high bits
  if (src2 < 0) {
    reg[d] |= (0xFFFFFFFF << (32 - total));
  }
}



// 0x2b - Shift Right Logical with Immediate (Immediate Format)
void shri() {
  if (verbose) {
    // Print a message indicating the operation, immediate value, and registers involved
    printf("shri  %x,r%x,r%x\n", imm16, s2, d);
  }

  // Perform a logical right shift of the value in register s2 by the number of bits specified in imm16,
  // and store the result in register d
  reg[d] = reg[s2] >> imm16;
}



// 0x2f - Shift Right Arithmetic with Immediate (Immediate Format)
void shrai() {
  if (verbose) {
    // Print a message indicating the operation, immediate value, and registers involved
    printf("shrai %x,r%x,r%x\n", imm16, s2, d);
  }

  // Perform an arithmetic right shift of the value in register s2 by the number of bits specified in imm16,
  // and store the result in register d
  reg[d] = reg[s2] >> imm16;
}


void unknown_op(){
  printf( "unknown instruction %08x\n", ir );
  printf( " op1=%x",  op1 );
  printf( " d=%x",    d );
  printf( " s1=%x",   s1 );
  printf( " s2=%x\n", s2 );
  printf( "program terminates\n" );
  exit( -1 );
}










int main( int argc, char **argv ){

  if( argc > 1 ){
    if( ( argv[1][0] == '-' ) && ( argv[1][1] == 't' ) ){
      verbose = 1;
    }else if( ( argv[1][0] == '-' ) && ( argv[1][1] == 'v' ) ){
      verbose = 2;
    }else{
      printf( "usage:\n");
      printf( "  %s for just execution statistics\n", argv[0] );
      printf( "  %s -t for instruction trace\n", argv[0] );
      printf( "  %s -v for instructions, registers, and memory\n", argv[0] );
      printf( "input is read as hex 32-bit values from stdin\n" );
      exit( -1 );
    }
  }

  get_mem();

  if( verbose ) printf( "instruction trace:\n" );
  while( !halt_flag ){

    if( verbose ) printf( "at %02x, ", fip );
    ir = mem[ fip >> 2 ];  /* adjust for word addressing of mem[] */
    xip = fip;
    fip = xip + 4;
    inst_fetches++;

    decode();
// Switch statement to select and execute a function based on the value of op1
switch (op1) {
    case 0x0:
      // If op1 is 0x0, call the halt() function
      halt();
      break;
    case 0x4:
      // If op1 is 0x4, call the ld_l() function
      ld_l();
      break;
    case 0x5:
      // If op1 is 0x5, call the ld_li() function
      ld_li();
      break;
    case 0x7:
      // If op1 is 0x7, call the st_l() function
      st_l();
      break;
    case 0x14:
      // If op1 is 0x14, call the btne() function
      btne();
      break;
    case 0x15:
      // If op1 is 0x15, call the btnei() function
      btnei();
      break;
    case 0x16:
      // If op1 is 0x16, call the bte() function
      bte();
      break;
    case 0x17:
      // If op1 is 0x17, call the btei() function
      btei();
      break;
    case 0x1a:
      // If op1 is 0x1a, call the br() function
      br();
      break;
    case 0x1c:
      // If op1 is 0x1c, call the bc() function
      bc();
      break;
    case 0x1e:
      // If op1 is 0x1e, call the bnc() function
      bnc();
      break;
    case 0x24:
      // If op1 is 0x24, call the adds() function
      adds();
      break;
    case 0x25:
      // If op1 is 0x25, call the addi() function
      addi();
      break;
    case 0x26:
      // If op1 is 0x26, call the subs() function
      subs();
      break;
    case 0x27:
      // If op1 is 0x27, call the subi() function
      subi();
      break;
    case 0x28:
      // If op1 is 0x28, call the shls() function
      shls();
      break;
    case 0x29:
      // If op1 is 0x29, call the shli() function
      shli();
      break;
    case 0x2a:
      // If op1 is 0x2a, call the shrs() function
      shrs();
      break;
    case 0x2b:
      // If op1 is 0x2b, call the shri() function
      shri();
      break;
    case 0x2e:
      // If op1 is 0x2e, call the shras() function
      shras();
      break;
    case 0x2f:
      // If op1 is 0x2f, call the shrai() function
      shrai();
      break;
    default:
      // If op1 doesn't match any of the cases, call the unknown_op() function
      unknown_op();
}



    if( ( verbose > 1 ) || ( halt_flag && ( verbose == 1 )) ){
      for( int i = 0; i < 8 ; i++ ){
        printf( "  r%x: %08x", i , reg[ i ] );
        printf( "  r%x: %08x", i + 8 , reg[ i + 8 ] );
        printf( "  r%x: %08x", i + 16, reg[ i + 16 ] );
        printf( "  r%x: %08x\n", i + 24, reg[ i + 24 ] );
      }
        printf("  cc: %d\n", cc_bit);

    }
  }

  if( verbose ) printf( "\n" );
  printf( "execution statistics (in decimal):\n" );
  printf( "  instruction fetches = %d\n", inst_fetches );
  printf( "  data words read     = %d\n", memory_reads );
  printf( "  data words written  = %d\n", memory_writes );
  printf( "  branches executed   = %d\n", branches );
  if( taken_branches == 0 ){
    printf( "  branches taken      = 0\n" );
  }else{
    printf( "  branches taken      = %d (%.1f%%)\n",
      taken_branches, 100.0*((float)taken_branches)/((float)branches) );
  }
  return 0;
}
