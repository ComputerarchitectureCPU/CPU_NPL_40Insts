
`include "N_bitReg.v"
`include "InstMem.v"
`include "regFile.v"
`include "immGen.v"
`include "NbitMUX.v"
`include "NbitALU.v"
`include "DataMem.v"
`include "ctrl_unit.v"
`include "nBitShifter.v"
`include "alu_ctrl.v"



module top(input clk, rst);

//wire [31:0] PC_in, PC_out, inst_out, imm_gen_out, alu_out, data_mem_out, Write_data, pc_inc, pc_branch,read1, read2, shift_out, alu_in_mux;
//wire [4:0] read1addr, read2addr, write_addr;
//wire zero_flag, err, Branch , MemRead , MemtoReg, MemWrite, RegWrite, ALUSrc, alusel;
//wire [1:0] ALUOp;

//RCA #(.n(32)) next_pc(.A(PC_out), .B(32'd4), .sum(PC_inc), .err(err));
//N_bitReg#(.n(32)) PC(.D(PC_in), .clk(clk), .rst(rst), .load(1'b1), .Q(PC_out));

//InstMem  inst(.addr(PC_out), .data_out(inst_out));

//regFile #(.n(5)) register(.rst(rst), .clk(clk), .RegWrite(RegWrite), .data(Write_data), .read1(read1addr), .read2(read2addr), .write(write_addr), .r1(read1), .r2(read2));

//immGen imm(.gen_out(imm_gen_out), .inst(inst_out));

//nBitShifter#(.n(32)) shl( .x(imm_gen_out), .y(shift_out));

//RCA #(.n(32)) branch_pc(.A(PC_out), .B(shift_out), .sum(pc_branch), .err(err));
//NbitMUX#(.n(32)) MUX_pc(.A(pc_inc), .B(PC_branch), .sel(zero_flag & Branch), .out(PC_in));


//NbitMUX#(.n(32)) MUX_reg(.A(read2), .B(imm_gen_out), .sel(ALUSrc), .out(alu_in_mux));
//NbitALU#(.N(32)) alu(.sel(alusel), .A(read1), .B(alu_in_mux), .ALUout(alu_out), .ZeroFlag(zero_flag));


//DataMem data(.clk(clk), .MemRead(MemRead), .MemWrite(MemWrite), .addr(alu_out[6:2]), .data_in(read2), .data_out(data_mem_out));
//NbitMUX#(.n(32)) MUX_data(.A(data_mem_out), .B(alu_out), .sel(MemtoReg), .out(Write_data));


//alu_ctrl alu_c(.aluop(ALUOp), .inst1(inst_out[14:12]), .inst2(inst_out[30]), .alusel(alusel));
//ctrl_unit cntrl(.inst(inst_out[6:0]), .Branch(Branch) , .MemRead(MemRead) , .MemtoReg(MemReg), .MemWrite(MemWrite), .ALUSrc(ALUSrc), .RegWrite(RegWrite), .ALUOp(ALUOp));

wire [31:0] Read_addr,PC_in,inst_out, write_data, read1, read2, gen_out, shift_out, alu_input,alu_out, mem_out, B_Add_Out, pc_inc, IF_ID_PC, IF_ID_inst_out, ID_EX_PC, ID_EX_RegR1, ID_EX_RegR2, ID_EX_Imm;
wire Branch, MemRead, MemtoReg, MemWrite, ALUSrc, RegWrite,zFlag, ofFlag, B_MUX_Sel, discard1, discard2;
wire [1:0] ALUOp;
wire [3:0] alu_Selection;
reg [12:0] num;



N_bitReg #(64) IF_ID ({Read_addr,inst_out},clk,rst,1'b1,{IF_ID_PC,IF_ID_inst_out});


wire ID_EX_Branch, ID_EX_MemRead, ID_EX_MemtoReg, ID_EX_MemWrite, ID_EX_ALUSrc, ID_EX_RegWrite; 
wire [1:0] ID_EX_ALUOp;
wire [3:0] ID_EX_Func;
wire [4:0] ID_EX_Rs1; 
wire [4:0] ID_EX_Rs2; 
wire [4:0] ID_EX_Rd;


N_bitReg #(155) ID_EX ({Branch, MemRead, MemtoReg, MemWrite, ALUSrc, RegWrite, ALUOp,IF_ID_PC,read1, read2,gen_out,{IF_ID_inst_out[30],IF_ID_inst_out[14:12]},IF_ID_inst_out[19:15], IF_ID_inst_out[24:20],IF_ID_inst_out[11:7]},clk,rst,1'b1,
{ID_EX_Branch, ID_EX_MemRead, ID_EX_MemtoReg, ID_EX_MemWrite, ID_EX_ALUSrc, ID_EX_RegWrite,ID_EX_ALUOp,ID_EX_PC,ID_EX_RegR1,ID_EX_RegR2,ID_EX_Imm, ID_EX_Func,ID_EX_Rs1,ID_EX_Rs2,ID_EX_Rd} );
 
 
 wire [31:0] EX_MEM_BranchAddOut;
 wire [31:0] EX_MEM_alu_out; 
 wire [31:0] EX_MEM_RegR2;
 wire EX_MEM_Branch, EX_MEM_MemRead, EX_MEM_MemtoReg, EX_MEM_MemWrite, EX_MEM_RegWrite;
 wire [4:0] EX_MEM_Rd;
 wire EX_MEM_Zero;
 

 N_bitReg #(107) EX_MEM (
  {B_Add_Out,alu_out,ID_EX_RegR2,ID_EX_Branch, ID_EX_MemRead, ID_EX_MemtoReg, ID_EX_MemWrite, ID_EX_RegWrite,ID_EX_Rd,zFlag },clk,rst,1'b1,{EX_MEM_BranchAddOut,EX_MEM_alu_out, EX_MEM_RegR2, EX_MEM_Branch, EX_MEM_MemRead, EX_MEM_MemtoReg, EX_MEM_MemWrite, EX_MEM_RegWrite, EX_MEM_Rd,  EX_MEM_Zero} );



wire [31:0] MEM_WB_data_mem_out;
wire [31:0] MEM_WB_alu_out;
wire MEM_WB_MemtoReg,MEM_WB_RegWrite;
wire [4:0] MEM_WB_Rd;


N_bitReg #(71) MEM_WB (
 {EX_MEM_MemtoReg,EX_MEM_RegWrite, mem_out,EX_MEM_alu_out,EX_MEM_Rd},clk,rst,1'b1,
 {MEM_WB_MemtoReg,MEM_WB_RegWrite,MEM_WB_data_mem_out, MEM_WB_alu_out,MEM_WB_Rd} );



N_bitReg #(32) rg (PC_in,clk,rst,1'b1,Read_addr);

InstMem IM(Read_addr[7:2], inst_out); 

ctrl_unit CU(IF_ID_inst_out[6:2], Branch, MemRead, MemtoReg, MemWrite, ALUSrc, RegWrite, ALUOp);
regFile #(5) RF( rst,  clk,MEM_WB_RegWrite,  write_data,ID_EX_Rs1,ID_EX_Rs2, MEM_WB_Rd,  read1, read2);
 
immGen IG(gen_out, IF_ID_inst_out);

nBitShifter #(32) SL(ID_EX_Imm,shift_out);

NbitMUX  #(32) MUX_RF(ID_EX_Imm,ID_EX_RegR2, ID_EX_ALUSrc, alu_input);

alu_ctrl ALU_CU(ID_EX_ALUOp, ID_EX_Func[2:0],ID_EX_Func[3], alu_Selection);

NbitALU #(32)ALU(alu_Selection,ID_EX_RegR1,alu_input, alu_out, zFlag);

DataMem DM( clk, EX_MEM_MemRead, EX_MEM_MemWrite, EX_MEM_alu_out[7:2], EX_MEM_RegR2,mem_out);

NbitMUX  #(32) MUX_Mem(MEM_WB_data_mem_out,MEM_WB_alu_out, MEM_WB_MemtoReg, write_data);

assign B_MUX_Sel = EX_MEM_Branch & EX_MEM_Zero;

RCA #(32) B_adder(
shift_out,
 ID_EX_PC, 
 B_Add_Out, discard1

);

RCA #(32) PC_adder(32'd4, Read_addr, pc_inc , discard2);

NbitMUX  #(32) MUX_PC(EX_MEM_BranchAddOut,pc_inc, B_MUX_Sel, PC_in);

endmodule


