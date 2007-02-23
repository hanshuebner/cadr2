--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:    14:50:58 11/11/05
-- Design Name:    
-- Module Name:    cadr2_cpu - Behavioral
-- Project Name:   
-- Target Device:  
-- Tool versions:  
-- Description:
--
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- Derived from caddr.v
-- 10/2005 brad parker brad@heeltoe.com
-- 
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

--
--   +++++++++++++++++++++++++++                    +--------
--   |                         |                    |
--   |                         |                    |
-- --+                         +--------------------+
--
--   ^                         ^
--   |                         |
--   |                      latch A&M memory output
--  latch IR
--
--

entity cadr2_cpu is 
port(
	clk : in std_logic;
	reset : in std_logic
	);
end cadr2_cpu;

--input int;
--inout[15:0] spy;
--input dbread_n, dbwrite_n;
--input[3:0] eadr;

architecture Behavioral of cadr2_cpu is

component cadr2_wcs
	port (
	addr : in std_logic_vector(13 downto 0);
	di : in std_logic_vector(48 downto 0);
	do : out std_logic_vector(48 downto 0);
	wr : in std_logic;
	en : in std_logic;
	clk : in std_logic);
end component;

component cadr2_ammem
port (
  a_addr	: in std_logic_vector(9 downto 0);
  a_di	: in std_logic_vector(31 downto 0);
  a_do	: out std_logic_vector(31 downto 0);
  a_wr	: in std_logic;
  a_en	: in std_logic;

  m_addr	: in std_logic_vector(4 downto 0);
  m_di	: in std_logic_vector(31 downto 0);
  m_do	: out std_logic_vector(31 downto 0);
  m_wr	: in std_logic;
  m_en	: in std_logic;

  clk		: in std_logic);
end component;

component cadr2_dispatch
port(
  a	: in std_logic_vector(10 downto 0);
  di	: in std_logic_vector(16 downto 0);
  do	: out std_logic_vector(16 downto 0);
  wr	: in std_logic;
  en	: in std_logic;
  clk	: in std_logic);
end component;

component cadr2_dmask
port (
  a	: in std_logic_vector(4 downto 0);
  do	: out std_logic_vector(6 downto 0));
end component;

component cadr2_maskleft
 port (
	mskl				: in std_logic_vector(4 downto 0);
	msk_left_out	: out std_logic_vector(31 downto 0));
end component;

component cadr2_maskright
 port (
	mskr				: in std_logic_vector(4 downto 0);
	msk_right_out	: out std_logic_vector(31 downto 0));
end component;

component cadr2_pdl
 port (
	addr	: in std_logic_vector(9 downto 0);
	di		: in std_logic_vector(31 downto 0);
	do		: out std_logic_vector(31 downto 0);
	wr		: in std_logic;
	en		: in std_logic;
	clk	: in std_logic);
end component;

component cadr2_spc
 port (
	addr	: in std_logic_vector(4 downto 0);
	di		: in std_logic_vector(18 downto 0);
	do		: out std_logic_vector(18 downto 0);
	wr		: in std_logic;
	wclk	: in std_logic);
end component;

component cadr2_vmem0
 port (
	addr	: in std_logic_vector(10 downto 0);
	di		: in std_logic_vector(4 downto 0);
	do		: out std_logic_vector(4 downto 0);
	wr		: in std_logic;
	en		: in std_logic;
	clk	: in std_logic);
end component;

component cadr2_vmem1
 port (
	addr	: in std_logic_vector(9 downto 0);
	di		: in std_logic_vector(23 downto 0);
	do		: out std_logic_vector(23 downto 0);
	wr		: in std_logic;
	en		: in std_logic;
	clk	: in std_logic);
end component;

component ic_74S181
 port (
	s			: in std_logic_vector(3 downto 0);
	a			: in std_logic_vector(3 downto 0);
	b			: in std_logic_vector(3 downto 0);
	m			: in std_logic;
	cin_n		: in std_logic;
	f			: out std_logic_vector(3 downto 0);
	x			: out std_logic;
	y			: out std_logic;
	cout_n	: out std_logic;
	aeb		: out std_logic);
end component;

component ic_74S182
 port (
	cin_n		: in std_logic;
	x			: in std_logic_vector(3 downto 0);
	y			: in std_logic_vector(3 downto 0);
	xout		: out std_logic;
	yout		: out std_logic;
	cout0_n	: out std_logic;
	cout1_n	: out std_logic;
	cout2_n	: out std_logic);
end component;

signal npc : std_logic_vector(13 downto 0);
signal dpc : std_logic_vector(13 downto 0);
signal ipc : std_logic_vector(13 downto 0);
signal spc : std_logic_vector(18 downto 0);

signal ir : std_logic_vector(48 downto 0);

signal a : std_logic_vector(31 downto 0);
signal a_latch : std_logic_vector(31 downto 0);


signal wadr : std_logic_vector(9 downto 0);
signal destd : std_logic;
signal destmd : std_logic;

signal apass : std_logic;
signal apass_n : std_logic;
signal amemenb_n : std_logic;
signal apassenb_n : std_logic;
signal awp : std_logic;

signal aadr : std_logic_vector(9 downto 0);

signal aeqm_bits : std_logic_vector(7 downto 0);
signal aeqm : std_logic;
signal alu : std_logic_vector(32 downto 0);

signal divposlasttime_n : std_logic;
signal divsubcond : std_logic;
signal divaddcond : std_logic;
--signal aluadd : std_logic;
--signal alusub : std_logic;
signal alusubadd : std_logic_vector(1 downto 0);	-- combined for easier assignment
signal mulnop_n : std_logic;
signal mul_n : std_logic;
signal div_n : std_logic;
signal specalu_n : std_logic;

signal divmul : std_logic_vector(1 downto 0);

signal iralu : std_logic;
signal osel : std_logic_vector(1 downto 0);

signal aluf : std_logic_vector(3 downto 0);
signal aluf_n : std_logic_vector(3 downto 0);
signal alumode_n : std_logic;
signal alumode : std_logic;
signal cin0_n : std_logic;

signal amem : std_logic_vector(31 downto 0);

signal aparok : std_logic;

signal dfall_n : std_logic;
signal dispenb : std_logic;
signal ignpopj_n : std_logic;
signal jfalse : std_logic;
signal jcalf : std_logic;
signal jretf : std_logic;
signal jret : std_logic;
signal iwrite : std_logic;
signal ipopj_n : std_logic;
signal popj_n : std_logic;
signal srcspcpopreal_n : std_logic;
signal spop_n : std_logic;
signal spush_n : std_logic;

signal popj : std_logic;

signal spcwpass_n : std_logic;
signal spcpass_n : std_logic;
signal swp : std_logic;
signal spcenb : std_logic;
signal spcdrive_n : std_logic;
signal spcnt_n : std_logic;

signal inop : std_logic;
signal spushd : std_logic;
signal iwrited : std_logic;
signal inop_n : std_logic;
signal spushd_n : std_logic;
signal iwrited_n : std_logic;
signal n : std_logic;
signal pcs1 : std_logic;
signal pcs0 : std_logic;

signal nopa_n : std_logic;
signal nopa : std_logic;
signal nop : std_logic;
signal nop_n : std_logic;

-- page DRAM0-2
signal dadr : std_logic_vector(10 downto 0);
signal dr : std_logic;
signal dp : std_logic;
signal dn : std_logic;
signal daddr0 : std_logic;
signal dmask : std_logic_vector(6 downto 0);
signal dwe : std_logic;
signal dmask_addr : std_logic_vector(4 downto 0);
signal disp_dout : std_logic_vector(16 downto 0);

-- page DSPCTL
signal dparh_n : std_logic;
signal dparl : std_logic;
signal dpareven : std_logic;
signal dparok : std_logic;
signal dmapbenb_n : std_logic;
signal dispwr : std_logic;
signal dc : std_logic_vector(9 downto 0);

-- page FLAG
signal statbit_n : std_logic;
signal ilong_n : std_logic;
signal aluneg : std_logic;
signal pgf_or_int : std_logic;
signal pgf_or_int_or_sb : std_logic;
signal sint : std_logic;

signal conds : std_logic_vector(2 downto 0);
signal jcond : std_logic;
signal lc_byte_mode : std_logic;
signal prog_unibus_reset : std_logic;
signal int_enable : std_logic;
signal sequence_break : std_logic;

-- page IOR
signal ib : std_logic_vector(47 downto 0);	-- renamed from iob
signal ob : std_logic_vector(31 downto 0);

-- page IPAR
signal ipar : std_logic_vector(3 downto 0);
signal iparity : std_logic;
signal iparok : std_logic;

-- page L
signal lparl : std_logic;
signal lparm_n : std_logic;
signal lparity : std_logic;
signal lparity_n : std_logic;

-- page LC
signal lc : std_logic_vector(25 downto 0);
signal lca : std_logic_vector(4 downto 0);
--signal lcry3 : std_logic;

signal lcdrive_n : std_logic;
signal sh4_n : std_logic;
signal sh3_n : std_logic;

signal mf : std_logic_vector(31 downto 0);

signal lc0b : std_logic;
signal next_instr : std_logic;
signal newlc_in_n : std_logic;
signal have_wrong_word : std_logic;
signal last_byte_in_word : std_logic;
signal needfetch : std_logic;
signal ifetch_n : std_logic;
signal spcmung : std_logic;
signal spc1a : std_logic;
signal lcinc : std_logic;
signal lcinc_n : std_logic;
signal newlc_n : std_logic;

signal newlc : std_logic;
signal sintr : std_logic;
signal next_instrd : std_logic;

signal int : std_logic;

signal lc_modifies_mrot_n : std_logic;
signal inst_in_left_half : std_logic;
signal inst_in_2nd_or_4th_quarter : std_logic;

signal wpc : std_logic_vector(13 downto 0);

-- page MCTL
signal mpass : std_logic;
signal mpass_n : std_logic;
signal mpassl_n : std_logic;
signal mpassm_n : std_logic;
signal srcm : std_logic;
signal mwp : std_logic;
signal madr : std_logic_vector(4 downto 0);

-- page MD
signal md : std_logic_vector(31 downto 0);
signal mdhaspar : std_logic;
signal mdpar : std_logic;
signal mddrive_n : std_logic;
signal mdgetspar : std_logic;
signal mdclk : std_logic;
signal mempar_in : std_logic;

signal loadmd : std_logic;
signal ignpar_n : std_logic;

-- page MDS
signal mds : std_logic_vector(31 downto 0);
signal mem : std_logic_vector(31 downto 0);
signal busint_bus : std_logic_vector(31 downto 0);
signal mempar_out : std_logic;

signal mdparodd : std_logic;

-- page MF
signal mfenb : std_logic;
signal mfdrive_n : std_logic;

-- page MLATCH
signal mmem_latched : std_logic_vector(31 downto 0);
signal mmemparity : std_logic;

signal mmemparok : std_logic;
signal m : std_logic_vector(31 downto 0);

-- page MMEM
signal mmem : std_logic_vector(31 downto 0);

signal mo : std_logic_vector(31 downto 0);

signal msk_right_out : std_logic_vector(31 downto 0);
signal msk_left_out : std_logic_vector(31 downto 0);
signal msk : std_logic_vector(31 downto 0);

signal dcdrive : std_logic;
signal opcdrive_n : std_logic;
signal zero16 : std_logic;
signal zero12_drive : std_logic;
signal zero16_drive : std_logic;
signal zero16_drive_n : std_logic;

-- page PDL
signal pdlparity : std_logic;
signal pdl : std_logic_vector(31 downto 0);

-- page PDLCTL
signal pdla : std_logic_vector(9 downto 0);
signal pdlp_n : std_logic;
signal pdlwrite : std_logic;
signal pwp : std_logic;
signal pdlenb : std_logic;
signal pdldrive_n : std_logic;
signal pdlcnt_n : std_logic;
signal imodd_n : std_logic;
signal destspcd : std_logic;
signal pdlwrited : std_logic;
signal pwidx_n : std_logic;
signal imodd : std_logic;
signal destspcd_n : std_logic;

-- page PDLPTR
signal pidrive : std_logic;
signal ppdrive_n : std_logic;
signal pdlidx : std_logic_vector(9 downto 0);

-- page Q
signal q : std_logic_vector(31 downto 0);
signal qs : std_logic_vector(1 downto 0);
signal srcq : std_logic;
signal qdrive : std_logic;

-- page SHIFT0-1
signal sa : std_logic_vector(31 downto 0);
signal r : std_logic_vector(31 downto 0);
signal rtmp : std_logic_vector(31 downto 0);

-- page SMCTL
signal mr_n : std_logic;
signal sr_n : std_logic;
--signal s1 : std_logic;
--signal s0 : std_logic;
signal mskr : std_logic_vector(4 downto 0);

--signal s4 : std_logic;
--signal s4_n : std_logic;
--signal s3 : std_logic;
--signal s2 : std_logic;
signal s : std_logic_vector(4 downto 0);
signal mskl : std_logic_vector(4 downto 0);

-- page SOURCE
signal irbyte_n : std_logic;
signal irdisp_n : std_logic;
signal irjump_n : std_logic;
signal iralu_n : std_logic;
signal irdisp : std_logic;
signal irjump : std_logic;

signal irflags : std_logic_vector(3 downto 0);

signal funct : std_logic_vector(3 downto 0);
signal funct2_n : std_logic;

signal srcq_n : std_logic;
signal srcopc_n : std_logic;
signal srcpdltop_n : std_logic;
signal srcpdlpop_n : std_logic;
signal srcpdlidx_n : std_logic;
signal srcpdlptr_n : std_logic;
signal srcspc_n : std_logic;
signal srcdc_n : std_logic;
signal srcspcpop_n : std_logic;
signal srclc_n : std_logic;
signal srcmd_n : std_logic;
signal srcmap_n : std_logic;
signal srcvma_n : std_logic;

signal srcflags : std_logic_vector(12 downto 0);

signal srclc : std_logic;
signal imod : std_logic;

signal destmem_n : std_logic;
signal destvma_n : std_logic;
signal destmdr_n : std_logic;
signal dest : std_logic;
signal destm : std_logic;
signal destintctl_n : std_logic;
signal destlc_n : std_logic;
signal destimod1_n : std_logic;
signal destimod0_n : std_logic;
signal destspc_n : std_logic;
signal destpdlp_n : std_logic;
signal destpdlx_n : std_logic;
signal destpdl_x_n : std_logic;
signal destpdl_p_n : std_logic;
signal destpdltop_n : std_logic;

signal destflags : std_logic_vector(9 downto 0);

signal destspc : std_logic;

-- page SPC
signal spcptr : std_logic_vector(4 downto 0);

signal spcw : std_logic_vector(18 downto 0);
signal spco : std_logic_vector(18 downto 0);

signal spcopar : std_logic;

-- page SPCPAR
signal spcwpar : std_logic;
signal spcwparl_n : std_logic;
signal spcwparh : std_logic;
signal spcparok : std_logic;

signal halt_n : std_logic;

signal mdparerr : std_logic;
signal parerr_n : std_logic;
signal memparok_n : std_logic;
signal memparok : std_logic;
signal trap_n : std_logic;
signal trap : std_logic;
signal boot_trap : std_logic;

signal mdpareven : std_logic;

-- page VCTRL1
signal memstart : std_logic;
signal mbusy_sync : std_logic;
signal memop_n : std_logic;
signal memprepare : std_logic;
signal memstart_n : std_logic;

signal wrcyc : std_logic;
signal wmapd : std_logic;
signal mbusy : std_logic;
signal rdcyc : std_logic;
signal pfw_n : std_logic;
signal pfr_n : std_logic;
signal vmaok_n : std_logic;
signal wmapd_n : std_logic;
signal memrq : std_logic;

signal set_rd_in_progess : std_logic;
signal mfinish_n : std_logic;
signal rd_in_progress : std_logic;

signal memack_n : std_logic;
signal memgrant_n : std_logic;

signal mfinishd_n : std_logic;
signal rdfinish_n : std_logic;
signal wait_n : std_logic;

-- page VCTRL2
signal mapwr0d : std_logic;
signal mapwr1d : std_logic;
signal vm0wp_n : std_logic;
signal vm1wp : std_logic;
signal vmaenb_n : std_logic;
signal vmasel : std_logic;
signal memdrive_n : std_logic;
signal mdsel : std_logic;
signal use_md : std_logic;
signal wmap_n : std_logic;
signal memwr_n : std_logic;
signal memrd_n : std_logic;

signal memflags : std_logic_vector(2 downto 0);

signal lm_drive_enb : std_logic;

signal wmap : std_logic;

-- page VMA
signal vma : std_logic_vector(31 downto 0);
signal vmadrive_n : std_logic;

-- page VMAS
signal vmas : std_logic_vector(31 downto 0);

signal mapi : std_logic_vector(23 downto 8);
signal mapi_n : std_logic_vector(12 downto 8);

signal vmap_n : std_logic_vector(4 downto 0);

-- page VMEM0 - virtual memory map stage 0
signal srcmap : std_logic;
signal use_map_n : std_logic;
signal vmoparck : std_logic;
signal v0parok : std_logic;
signal vm0pari : std_logic;
signal vmoparodd : std_logic;

signal vmopar : std_logic;

signal vmo_n : std_logic_vector(23 downto 0);
signal vmo : std_logic_vector(23 downto 0);


signal vmap : std_logic_vector(4 downto 0);

signal vm1mpar : std_logic;
signal vm1lpar : std_logic;

signal mapdrive_n : std_logic;
signal vm0par : std_logic;
signal vm0parm : std_logic;
signal vm0parl : std_logic;
signal adrpar_n : std_logic;

signal i : std_logic_vector(48 downto 0);
signal iram : std_logic_vector(48 downto 0);
--signal spy_ir : std_logic_vector(47 downto 0);

signal ramdisable : std_logic;
signal promdisabled_n : std_logic;

signal opcinh : std_logic;
signal opcclk : std_logic;
signal lpc_hold : std_logic;
signal opcinh_n : std_logic;
signal opcclk_n : std_logic;

signal ldstat : std_logic;
signal idebug : std_logic;
signal nop11 : std_logic;
signal step : std_logic;
signal ldstat_n : std_logic;
signal idebug_n : std_logic;
signal nop11_n : std_logic;
signal step_n : std_logic;

signal run : std_logic;

signal machrun : std_logic;
signal ssdone_n : std_logic;
signal stat_ovf : std_logic;
signal stathalt_n : std_logic;

signal spcoparok : std_logic;
signal vm0parok : std_logic;
signal pdlparok : std_logic;

signal lowerhighok_n : std_logic;
signal highok : std_logic;
signal ldmode : std_logic;
signal prog_reset_n : std_logic;
signal reset_n : std_logic;
signal err : std_logic;
signal errhalt_n : std_logic;
signal bus_reset_n : std_logic;
signal bus_power_reset_n : std_logic;
signal power_reset : std_logic;
signal clock_reset_n : std_logic;
--signal prog_boot std_logic;
signal boot_n : std_logic;

signal prog_bus_reset : std_logic;

signal busint_lm_reset_n : std_logic;

signal opcclka : std_logic;

-- page PCTL

signal mparity : std_logic;
signal pdl_latch : std_logic_vector(31 downto 0);

-- page SPCLCH
signal spco_latched : std_logic_vector(18 downto 0);
signal spcpar : std_logic;

-- page OLORD1 
signal promdisable : std_logic;
signal trapenb : std_logic;
signal stathenb : std_logic;
signal errstop : std_logic;
signal speed1 : std_logic;
signal speed0 : std_logic;

signal srun : std_logic;
signal sstep : std_logic;
signal ssdone : std_logic;
signal promdisabled : std_logic;

signal speed0a : std_logic;
signal speed1a : std_logic;
signal sspeed0 : std_logic;
signal sspeed1 : std_logic;

-- page OLORD2

signal ape_n : std_logic;
signal mpe_n : std_logic;
signal pdlpe_n : std_logic;
signal dpe_n : std_logic;
signal ipe_n : std_logic;
signal spe_n : std_logic;
signal higherr_n : std_logic;
signal mempe_n : std_logic;

signal v0pe_n : std_logic;
signal v1pe_n : std_logic;
signal statstop : std_logic;
signal halted_n : std_logic;

-- page L
signal l : std_logic_vector(31 downto 0);

-- page NPC
signal pc : std_logic_vector(13 downto 0);

-- page OPCS
signal opc : std_logic_vector(13 downto 0);

-- page PDLPTR
signal pdlptr : std_logic_vector(9 downto 0);

-- page SPCW
signal reta : std_logic_vector(13 downto 0);

signal mcycle_delay : std_logic_vector(9 downto 0);

-- page IWR
signal iwr : std_logic_vector(48 downto 0);

signal lpc : std_logic_vector(13 downto 0);

signal lvmo_n : std_logic_vector(23 downto 22);
signal pma : std_logic_vector(21 downto 8);

-- SPY 0

--signal spy_obh_n std_logic;
--signal spy_obl_n std_logic;
--signal spy_pc_n std_logic;
--signal spy_opc_n std_logic;
--signal spy_nc_n std_logic;
--signal spy_irh_n std_logic;
--signal spy_irm_n std_logic;
--signal spy_irl_n std_logic;

--signal spy_sth_n std_logic;
--signal spy_stl_n std_logic;
--signal spy_ah_n std_logic;
--signal spy_al_n std_logic;
--signal spy_mh_n std_logic;
--signal spy_ml_n std_logic;
--signal spy_flag2_n std_logic;
--signal spy_flag1_n std_logic;

--signal ldmode_n std_logic;
--signal ldopc_n std_logic;
--signal ldclk_n std_logic;
--signal lddbirh_n std_logic;
--signal lddbirm_n std_logic;
--signal lddbirl_n std_logic;

-- clocks
signal MCLK : std_logic;
--signal clk0_n : std_logic;
signal mclk0_n :std_logic;

signal osc0 : std_logic;
signal hifreq1 : std_logic;
signal hifreq2 : std_logic;
signal hf_n : std_logic;
signal hfdlyd : std_logic;
signal hftomm : std_logic;
signal clk_n : std_logic;
signal tpclk : std_logic;
signal tpclk_n : std_logic;
signal lclk : std_logic;
signal lclk_n : std_logic;
signal wp : std_logic;
signal lwp_n : std_logic;
signal tse : std_logic;
signal ltse : std_logic;
signal ltse_n : std_logic;
signal tpr0_n : std_logic;
signal tpr0a : std_logic;
signal tpr0d : std_logic;
signal tpr0d_n : std_logic;
signal tpr1a : std_logic;
signal tpr1d_n : std_logic;
signal tpwp : std_logic;
signal sone_n : std_logic;
signal tprend_n : std_logic;

signal tpr1 : std_logic;
signal tpr1_n : std_logic;
signal ff1d : std_logic;
signal tpr6_n : std_logic;
signal tpr5 : std_logic;
signal tpr5_n : std_logic;
signal tpr4 : std_logic;
signal tpr4_n : std_logic;
signal tpr3 : std_logic;
signal tpr3_n : std_logic;
signal tpr2 : std_logic;
signal tpr2_n : std_logic;
signal tpw0 : std_logic;
signal tpw0_n : std_logic;
signal tpw1 : std_logic;
signal tpw1_n : std_logic;
signal tpw2 : std_logic;
signal tpw2_n : std_logic;
signal tpw3 : std_logic;
signal tpw3_n : std_logic;

signal tpwpor1 : std_logic;
signal tpwpiram : std_logic;
signal tptse : std_logic;
signal tptsef : std_logic;
signal tptsef_n : std_logic;
signal maskc : std_logic;
signal ff1 : std_logic;
signal ff1_n : std_logic;
signal tendly_n : std_logic;
signal hangs_n : std_logic;
signal crbs_n : std_logic;
signal hang_n : std_logic;
signal iwe : std_logic;

signal sspeed1a : std_logic;
signal sspeed0a : std_logic;

-- Extras

signal vmem1_adr : std_logic_vector(9 downto 0);
signal opc_inh_or_clka : std_logic;
signal qclk : std_logic;
--signal ldopc std_logic;
signal ldclk : std_logic;

-- ALU extras

signal cin32_n : std_logic;
signal cin28_n : std_logic;
signal cin24_n : std_logic;
signal cin20_n : std_logic;
signal cin16_n : std_logic;
signal cin12_n : std_logic;
signal cin8_n : std_logic;
signal cin4_n : std_logic;

signal xout31 : std_logic;
signal xout27 : std_logic;
signal xout23 : std_logic;
signal xout19 : std_logic;
signal xout15 : std_logic;
signal xout11 : std_logic;
signal xout7 : std_logic;
signal xout3 : std_logic;

signal yout31 : std_logic;
signal yout27 : std_logic;
signal yout23 : std_logic;
signal yout19 : std_logic;
signal yout15 : std_logic;
signal yout11 : std_logic;
signal yout7 : std_logic;
signal yout3 : std_logic;

signal xx0 : std_logic;
signal xx1 : std_logic;

signal yy0 : std_logic;
signal yy1 : std_logic;

signal carry0_x : std_logic_vector(3 downto 0);
signal carry1_x : std_logic_vector(3 downto 0);
signal carry2_x : std_logic_vector(3 downto 0);

signal carry0_y : std_logic_vector(3 downto 0);
signal carry1_y : std_logic_vector(3 downto 0);
signal carry2_y : std_logic_vector(3 downto 0);

signal alu3_a : std_logic_vector(3 downto 0);
signal alu3_b : std_logic_vector(3 downto 0);
signal alu3_f : std_logic_vector(3 downto 0);

begin

-- page actl

-- XXX do we need this when we have dual port memory

do_wadr: process(clk)
  begin
    -- wadr 9  8  7  6  5  4  3  2  1  0
    --      0  0  0  0  0  18 17 16 15 14
    -- ir   23 22 21 20 19 18 17 16 15 14
		if clk'event and clk='1' then
			if destm='1' then
				wadr <= "00000" & ir(18 downto 14);
			else
				wadr <= ir(23 downto 14);
			end if;
			destd <= dest;
			destmd <= destm;
		end if;
end process do_wadr;

--apass <= destd & (ir(41 downto 32) = wadr(9 downto 0) ? 1'b1 : 1'b0); XXX
apass_n <= not apass;

amemenb_n <= not (apass_n and tse);
apassenb_n <= not (apass and tse);

awp <= destd and wp;

aadr <= ir(41 downto 32) when clk='1' else wadr;

-- page ALATCH

-- AML
-- transparent latch
--always @(CLK or amem or negedge reset_n)
do_aml: process(clk)
	begin
		if clk'event and clk='1' then
			if reset='1' then
				a_latch <= X"00000000";
			else
				a_latch <= amem;
			end if;
		end if;
	end process do_aml;

a <= a_latch when amemenb_n='0' else
		l when apassenb_n='0' else
		X"FFFFFFFF";

-- page ALU0-1

-- 74181 pulls down AEB if not equal
-- aeqm is the simulated open collector
aeqm <= '1' when (aeqm_bits = "11111111") else '0';

i_alu1_2a03 : ic_74S181 port map (
  b => alu3_b,
  a => alu3_a,
  s => aluf(3 downto 0),
  cin_n => cin32_n,
  m => alumode,
  f => alu3_f);

alu3_b	<= "000" & a(31);
alu3_a	<= "000" & m(31);
alu(32)	<= alu3_f(0);

i_alu1_2a08 : ic_74S181  port map (
  b => a(31 downto 28),
  a => m(31 downto 28),
  s => aluf(3 downto 0),
  cin_n => cin28_n,
  m => alumode,
  f => alu(31 downto 28),
  aeb => aeqm_bits(7),
  x => xout31,
  y => yout31);

i_alu1_2b08 : ic_74S181 port map (
  b => a(27 downto 24),
  a => m(27 downto 24),
  s => aluf(3 downto 0),
  cin_n => cin24_n,
  m => alumode,
  f => alu(27 downto 24),
  aeb => aeqm_bits(6),
  x => xout27,
  y => yout27);

i_alu1_2a13 : ic_74S181 port map (
  b => a(23 downto 20),
  a => m(23 downto 20),
  s => aluf(3 downto 0),
  cin_n => cin20_n,
  m => alumode,
  f => alu(23 downto 20),
  aeb => aeqm_bits(5),
  x => xout23,
  y => yout23);

i_alu1_2b13 : ic_74S181 port map (
  b => a(19 downto 16),
  a => m(19 downto 16),
  s => aluf(3 downto 0),
  cin_n => cin16_n,
  m => alumode,
  f => alu(19 downto 16),
  aeb => aeqm_bits(4),
  x => xout19,
  y => yout19);

i_alu0_2a23 : ic_74S181 port map (
  a => m(15 downto 12),
  b => a(15 downto 12),
  s => aluf(3 downto 0),
  cin_n => cin12_n,
  m => alumode,
  f => alu(15 downto 12),
  aeb => aeqm_bits(3),
  x => xout15,
  y => yout15);

i_alu0_2b23 : ic_74S181 port map (
  a => m(11 downto 8),
  b => a(11 downto 8),
  s => aluf(3 downto 0),
  cin_n => cin8_n,
  m => alumode,
  f => alu(11 downto 8),
  aeb => aeqm_bits(2),
  x => xout11,
  y => yout11);

i_alu0_2a28 : ic_74S181 port map (
  a => m(7 downto 4),
  b => a(7 downto 4),
  s => aluf(3 downto 0),
  cin_n => cin4_n,
  m => alumode,
  f => alu(7 downto 4),
  aeb => aeqm_bits(1),
  x => xout7,
  y => yout7);

i_alu0_2b28 : ic_74S181 port map (
  a => m(3 downto 0),
  b => a(3 downto 0),
  s => aluf(3 downto 0),
  cin_n => cin0_n,
  m => alumode,
  f => alu(3 downto 0),
  aeb => aeqm_bits(0),
  x => xout3,
  y => yout3);

-- page ALUC4

i_aluc4_2a20 : ic_74S182 port map (
  y => carry0_y,
  x => carry0_x,
  cout2_n => cin12_n,
  cout1_n => cin8_n,
  cout0_n => cin4_n,
  cin_n => cin0_n,
  xout => xx0,
  yout => yy0);

carry0_x <= xout15 & xout11 & xout7 & xout3;
carry0_y <= yout15 & yout11 & yout7 & yout3;

i_aluc4_2a19 : ic_74S182 port map (
  y => carry1_y,
  x => carry1_x,
  cout2_n => cin28_n,
  cout1_n => cin24_n,
  cout0_n => cin20_n,
  cin_n => cin16_n,
  xout => xx1,
  yout => yy1);

carry1_x <= xout31 & xout27 & xout23 & xout19;
carry1_y <= yout31 & yout27 & yout23 & yout19;

i_aluc4_2a18 : ic_74S182 port map (
  y => carry2_y,
  x => carry2_x,
  cout1_n => cin32_n,
  cout0_n => cin16_n,
  cin_n => cin0_n);

carry2_x	<= "00" & xx1 & xx0;
carry2_y <= "00" & yy1 & yy0;


divposlasttime_n <= not (q(0) or ir(6));

divsubcond <= not (div_n or divposlasttime_n);

divaddcond <= not (div_n or not (ir(5) or divposlasttime_n));

alusubadd(0) <= not (not (divaddcond and not a(31)) and
		  not(divsubcond and a(31)) and mul_n);

mulnop_n <= mul_n or q(0);

alusubadd(1) <= not (mulnop_n and not(not a(31) and divsubcond) and
	          not (divaddcond and a(31)) and irjump_n);

osel(1) <= not (not ir(13) or iralu_n);
osel(0) <= not (not ir(12) or iralu_n);

with alusubadd select
	aluf_n <= (not ir(3) & not ir(4) & ir(6) & ir(5)) when "00",
				"0110" when "01",
				"1001" when "10",
				"0000" when others;

with alusubadd select
	alumode_n <= ir(7) when "00", '1' when "01", '1' when "10", '0' when others;

with alusubadd select
	cin0_n <= not ir(2) when "00", '1' when "01", irjump when "10", '0' when others;

aluf <= not aluf_n;
alumode <= not alumode_n;

-- use ir(9)='1' as mode switch for floating point operations


-- page AMEM0-1

i_ammem: cadr2_ammem port map(aadr, l, amem, awp, '1', madr, l, mmem, mwp, '1', clk);

aparok <= '1';

-- page CONTRL

dfall_n <= not (dr and dp);					-- push-pop fall through

dispenb <= irdisp and funct2_n;
ignpopj_n <= irdisp_n or dr;

jfalse <= irjump and ir(6);					-- jump and inverted-sense

jcalf <= jfalse and ir(8);						-- call and inverted-sense

jret <= irjump and not ir(8) and ir(9);	-- return

jretf <= jret and ir(6);						-- return and inverted-sense

iwrite <= irjump and ir(8) and ir(9);		-- microcode write

ipopj_n <= not (ir(42) and nop_n);
popj_n <= ipopj_n and iwrited_n;

popj <= not popj_n;

srcspcpopreal_n <= srcspcpop_n or nop;

spop_n <= not (
	( not (srcspcpopreal_n and popj_n ) and ignpopj_n ) or
	(dispenb and dr and not dp) or
	(jret and not ir(6) and jcond) or
	(jretf and not jcond)
	);

spush_n <= not (destspc or (jcalf and not jcond) or
	(dispenb and dp and not dr) or
	(irjump and not ir(6) and ir(8) and jcond));

spcwpass_n <= not (spushd and tse);
spcpass_n <= not (spushd_n and tse);

swp <= spushd and wp;
spcenb <= not (srcspc_n and srcspcpop_n);
spcdrive_n <= not (spcenb and tse);
spcnt_n <= spush_n and spop_n;

inop_n <= not inop;
spushd_n <= not spushd;
iwrited_n <= not iwrited;

do_control: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			inop <= '0';
			spushd <= '0';
			iwrited <= '0';
		else
			inop <= n;
			spushd <= not spush_n;
			iwrited <= iwrite;
		end if;
	end if;
end process do_control;

-- select new pc
-- {pcs1,pcs0}
-- 00 0 spc
-- 01 1 ir
-- 10 2 dpc
-- 11 3 ipc

pcs1 <= not (
	(popj and ignpopj_n) or				-- popj & ignore
	(jfalse and not jcond) or				-- jump & invert & cond-not-satisfied
	(irjump and not ir(6) and jcond) or	-- jump & !invert & cond-satisfied
	(dispenb and dr and not dp)				-- dispatch + return & !push
	);

pcs0 <= not(
	(popj) or
	(dispenb and dfall_n) or
	(jretf and not jcond) or
	(jret and not ir(6) and jcond)
	);

-- N set if:
--  iwrite (microcode write)
--  dispatch & disp-N
--  jump & invert-jump-selse & cond-false & !next
--  jump & !invert-jump-sense & cond-true & !next

n <= not (trap_n and not ((iwrited) or
	    (dispenb and dn) or
	    (jfalse and not jcond and ir(7)) or
	    (irjump and not ir(6) and jcond and ir(7)))
	 );

nopa_n <= inop_n and nop11_n;
nopa  <= not nopa_n;

nop <= not (trap_n and nopa_n);
nop_n <= not nop;

-- page DRAM0-2

-- dadr  10 9  8  7  6  5  4  3  2  1  0
-- -------------------------------------
-- ir    22 21 20 19 18 17 16 15 14 13 d
-- dmask x  x  x  x  6  5  4  3  2  1  x
-- r     x  x  x  x  6  5  4  3  2  1  x

daddr0 <= (ir(8) and vmo(18)) or (ir(9) and vmo(19)) or
	(dmapbenb_n and dmask(0) and r(0)) or (ir(12));

dadr <= (ir(22 downto 13) & daddr0) or
			(("0000" & dmask(6 downto 1) & "0") and
			("0000" & r(6 downto 1) & "0"));

dwe <= dispwr and wp;

i_dram: cadr2_dispatch port map(dadr, a(16 downto 0), disp_dout, dwe, '1', clk);

dr <= disp_dout(16);
dp <= disp_dout(15);
dn <= disp_dout(14);
dpc <= disp_dout(13 downto 0);

-- page DSPCTL

dparh_n <= '1';
dparl <= '0';
dpareven <= dparh_n xor dparl;
dparok <= not (dpareven and dispenb);
dmapbenb_n <= not (ir(8) or ir(9));
dispwr <= not (irdisp_n or funct2_n);

do_dc: process(clk)
begin
	if clk'event and clk='1' then
		if irdisp_n = '0' then
			dc <= ir(41 downto 32);
		end if;
	end if;
end process do_dc;

--part_32x8prom
i_dmask: cadr2_dmask port map (dmask_addr, dmask);

dmask_addr <= "00" & ir(7) & ir(6) & ir(5);

-- page FLAG

statbit_n <= not (nopa_n and ir(46));
ilong_n  <= not (nopa_n and ir(45));

aluneg <= not (aeqm or not alu(32));

sint <= sintr and int_enable;

pgf_or_int <= vmaok_n or sint;
pgf_or_int_or_sb <= vmaok_n or sint or sequence_break;

conds <= ir(2 downto 0) and (ir(5) & ir(5) & ir(5));

with conds select
	jcond <= r(0) when "000",
				aluneg when "001",
				alu(32) when "010",
				aeqm when "011",
				vmaok_n when "100",
				pgf_or_int when "101",
				pgf_or_int_or_sb when "110",
				'1' when "111",
				'0' when others;

do_flags: process(clk)
begin
	if clk'event and clk='1' then
		if (reset_n = '0') then
			lc_byte_mode <= '0';
			prog_unibus_reset <= '0';
			int_enable <= '0';
			sequence_break <= '0';
		elsif destintctl_n = '0' then
			lc_byte_mode <= ob(29);
			prog_unibus_reset <= ob(28);
			int_enable <= ob(27);
			sequence_break <= ob(26);
      end if;
	end if;
end process do_flags;

-- page IOR

-- ib 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26
-- i   47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26
-- ob  21 20 19 18 17 16 15 14 13 12 11 10 9  8  7  6  5  4  3  2  1  0  

-- ib 25 24 ... 1  0
-- i   25 24 ... 1  0
-- ob  25 24 ... 1  0

ib <= i(47 downto 0) or (ob(21 downto 0) & ob(25 downto 0));

-- page IPAR

ipar <= "0000";
iparity <= '0';
iparok <= imodd or iparity;

-- page IREG

do_ir: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			ir <= "0000000000000000000000000000000000000000000000000";
		else
			if destimod1_n='1' then
				ir(47 downto 26) <= i(47 downto 26);
			else
				ir(47 downto 26) <= ib(47 downto 26);
			end if;

			if destimod0_n='1' then
				ir(25 downto 0) <= i(25 downto 0);
			else
				ir(25 downto 0) <= ib(25 downto 0);
			end if;
		end if;
	end if;
end process do_ir;

-- page IWR

do_iwr: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			iwr <= "0000000000000000000000000000000000000000000000000";
		else
			iwr(48) <= '0';
			iwr(47 downto 32) <= a(15 downto 0);
			iwr(31 downto 0) <= m(31 downto 0);
		end if;
	end if;
end process do_iwr;

-- page L

do_l: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			l <= X"00000000";
		else
			l <= ob;
		end if;
	end if;
end process do_l;

lparl <= '0';
lparm_n <= '0';
lparity <= '0';
lparity_n <= '1';


-- page LC

do_lc: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			lc <= "00000000000000000000000000";
		elsif destlc_n='0' then
			lc <= ob(25 downto 4) & ob(3 downto 0);
		else
			lc <= (lc(25 downto 4) + lca(4)) & lca(3 downto 0);
		end if;
	end if;
end process do_lc;

lca <= lc(3 downto 0) + ("000" & not(lcinc_n or lc_byte_mode)) + lcinc;

lcdrive_n <= not (srclc and tse);

-- xxx
-- I think the above is really
-- 
-- always @(posedge CLK)
--   begin
--     if (destlc_n == 0)
--       lc <= ob;
--     else
--       lc <= lc + 
--             !(lcinc_n | lc_byte_mode) ? 1 : 0 +
--             lcinc ? 1 : 0;
--
--   end
--

-- mux MF
mf <=	(needfetch & "0" & lc_byte_mode & prog_unibus_reset &
					int_enable & sequence_break & lc(25 downto 1) & lc0b) when lcdrive_n='0' else
					("000000000000000000" & opc(13 downto 0)) when opcdrive_n='0' else
-- zero16_drive drives top 16 bits to zero
-- zero12_drive drives top 4 bits of lower 16 to zero
-- don't need this since we don't pull up mf bus
--        zero12_drive ?
--	  { 16'b0, 4'b0, 12'b0 } :
					("0000000000000000000000" & dc(9 downto 0)) when dcdrive='1' else
					("0000000000000000000000" & pdlptr(9 downto 0)) when ppdrive_n='0' else
					("0000000000000000000000" & pdlidx(9 downto 0)) when pidrive='1' else
					q when qdrive='1' else
					md when mddrive_n='0' else
					l when mpassl_n='0' else
					vma when vmadrive_n='0' else
					(pfw_n & pfr_n & "1" & vmap_n(4 downto 0) & vmo(23 downto 0)) when mapdrive_n='0' else
					X"00000000";


-- page LCC

lc0b <= lc(0) and lc_byte_mode;
next_instr <= not(spop_n or not(srcspcpopreal_n and spc(14)));

newlc_in_n <= not(have_wrong_word and lcinc_n);
have_wrong_word <= not(newlc_n and destlc_n);
last_byte_in_word <= not(lc(1) or lc0b);
needfetch <= have_wrong_word or last_byte_in_word;

ifetch_n <= not (needfetch and lcinc);
spcmung <= spc(14) and not needfetch;
spc1a <= spcmung or spc(1);

lcinc <= next_instrd or (irdisp and ir(24));
lcinc_n <= not (next_instrd or (irdisp and ir(24)));

do_instrd: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			newlc <= '0';
			sintr <= '0';
			next_instrd <= '0';
		else
			newlc <= newlc_in_n;
			sintr <= int;
			next_instrd <= next_instr;
		end if;
	end if;
end process do_instrd;

newlc_n <= not newlc;

-- mustn't depend on nop

lc_modifies_mrot_n <= not (ir(10) and ir(11));

inst_in_left_half <= not ((lc(1) xor lc0b) or lc_modifies_mrot_n);

sh4_n <= inst_in_left_half xor not ir(4);

-- LC<1:0>
-- +---------------+
-- | 0 | 3 | 2 | 1 |
-- +---------------+
-- |   0   |   2   |
-- +---------------+

inst_in_2nd_or_4th_quarter <=
	not (lc(0) or lc_modifies_mrot_n) and lc_byte_mode;

sh3_n <= not ir(3) xor inst_in_2nd_or_4th_quarter;

--page LPC

do_lpc: process(clk)
begin
	if clk'event and clk='1' then
		if lpc_hold='0' then
			lpc <= pc;
		end if;
	end if;
end process do_lpc;

-- dispatch and instruction as N set
wpc <= lpc when (irdisp and ir(25))='1' else pc;

-- page MCTL

mpass <= '1' when ("1" & ir(30 downto 26)) = (destmd & wadr(4 downto 0)) else '0';
mpass_n <= not mpass;

mpassl_n <= not (mpass and tse and not ir(31));
mpassm_n <= not (mpass_n and tse and not ir(31));

srcm <= not ir(31) and mpass_n;

mwp <= destmd and tpwp;

madr <= ir(30 downto 26) when clk='1' else wadr(4 downto 0);

-- page MD

do_md: process(mdclk)
begin
	if mdclk'event and mdclk='1' then
		if reset_n='0' then
			md <= X"00000000";
		else
			md <= mds;
			mdhaspar <= mdgetspar;
			mdpar <= mempar_in;
		end if;
	end if;
end process do_md;

mddrive_n <= not (not srcmd_n and tse);
mdgetspar <= ignpar_n and destmdr_n;
mdclk <= not (loadmd or (not clk and not destmdr_n));

-- page MDS

mds <= ob when mdsel='1' else mem;

mdparodd <= '1';

mempar_out <= mdparodd;

-- mux MEM
mem <= 	md when memdrive_n='0' else
			busint_bus when loadmd='1' else
			X"00000000";

-- page MF
mfenb <= not srcm and not(spcenb or pdlenb);
mfdrive_n <= not(mfenb and tse);

-- page MLATCH

mmemparity <= '0';

-- transparent latch
do_mlatch: process(clk)
begin
	if clk'event and clk='1' then
		mmem_latched <= mmem;
		mparity <= mmemparity;
	end if;
end process do_mlatch;

mmemparok <= '1';

-- mux M
m <= mmem_latched when mpassm_n='0' else
		pdl_latch when pdldrive_n='0' else
		("000" & spcptr & "00000" & spco_latched) when spcdrive_n='0' else
		mf when mfdrive_n='0' else
		X"00000000";

-- page MMEM

-- combined with dual-port memory

-- page MO

--for (i = 0; i < 31; i++)
--  assign mo[i] =
--	osel == 2'b00 ? (msk[i] ? r[i] : a[i]) : a[i];

-- msk r  a       (msk&r)|(~msk&a)
--  0  0  0   0      0 0  0
--  0  0  1   1      0 1  1
--  0  1  0   0      0 0  0
--  0  1  1   1      0 1  1
--  1  0  0   0      0 0  0 
--  1  0  1   0      0 0  0
--  1  1  0   1      1 0  1 
--  1  1  1   1      1 0  1

-- masker output 
mo <= (msk and r) or (not msk and a);

with osel select
	ob <= alu(31 downto 0) when "01",
			alu(32 downto 1) when "10",
			alu(30 downto 0) & q(31) when "11",
			mo when others;

-- page MSKG4

--part_32x32prom_maskleft
i_mskl: cadr2_maskleft port map (mskl, msk_left_out);

--part_32x32prom_maskright
i_mskr: cadr2_maskright port map (mskr, msk_right_out);

msk <= msk_right_out and msk_left_out;

-- page NPC

npc <= "00000000000000" when trap='1' else
			(spc(13 downto 2) & spc1a & spc(0)) when pcs1='0' and pcs0='0' else
			ir(25 downto 12) when pcs1='0' and pcs0='1' else
			dpc when pcs1='1' and pcs0='0';
         -- ipc when pcs1='1 and pcs0='1';

do_npc: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			pc <= "00000000000000";
		else
			pc <= npc;
		end if;
	end if;
end process do_npc;

ipc <= pc + 1;

-- page OPCD

dcdrive <= not srcdc_n and tse;
opcdrive_n <= not(not srcopc_n and tse);

zero16 <= not(srcopc_n and srcpdlidx_n and srcpdlptr_n and srcdc_n);

zero12_drive <= zero16 and srcopc_n and tse;
zero16_drive <= zero16 and tse;
zero16_drive_n <= not (zero16 and tse);

-- page PDL

pdlparity <= '0';

--part_1kx32ram
i_pdl: cadr2_pdl port map (pdla, l, pdl, pwp, '1', clk);

-- page PDLCTL

pdla <= pdlidx when pdlp_n='1' else pdlptr;

pdlp_n <= not((clk and ir(30)) or (not CLK and pwidx_n));
pdlwrite <= not(destpdltop_n and destpdl_x_n and destpdl_p_n);

do_pdlctl: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			pdlwrited <= '0';
			pwidx_n <= '0';
			imodd <= '0';
			destspcd_n <= '0';
		else
			pdlwrited <= pdlwrite;
			pwidx_n <= destpdl_x_n;
			imodd <= imod;
			destspcd_n <= destspc_n;
		end if;
	end if;
end process do_pdlctl;

imodd_n <= not imodd;

destspcd <= not destspcd_n;

pwp <= pdlwrited and wp;

pdlenb <= not (srcpdlpop_n and srcpdltop_n);
pdldrive_n <= not (pdlenb and tse);

pdlcnt_n <= (srcpdlpop_n or nop) and destpdl_p_n;

-- page PDLPTR

pidrive <= tse and not srcpdlidx_n;
ppdrive_n <= not(tse and not srcpdlptr_n);

do_pdlptr: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			pdlidx <= "0000000000";
			pdlptr <= "0000000000";
		elsif destpdlx_n='0' then
			pdlidx <= ob(9 downto 0);
		elsif destpdlp_n='0' then
			pdlptr <= ob(9 downto 0);
		else
			if pdlcnt_n='0' then
				if srcpdlpop_n='1' then
					pdlptr <= pdlptr - 1;
				else
					pdlptr <= pdlptr + 1;
				end if;
			end if;
		end if;
   end if;
end process do_pdlptr;

-- page PLATCH

-- transparent latch

do_platch: process(clk)
begin
	if clk'event and clk='1' then
		pdl_latch <= pdl;
		mparity <= pdlparity;
	end if;
end process do_platch;

-- page Q

qs(1) <= not (not ir(1) or iralu_n);
qs(0) <= not (not ir(0) or iralu_n);

srcq <= not srcq_n;
qdrive <= srcq and tse;

--assign #1 QCLK = CLK;
qclk <= tpw2;

do_q: process(qclk)
begin
	if qclk'event and qclk='1' then
		if reset_n='0' then
			q <= X"00000000";
		elsif (qs(1)='1' or qs(0)='1') then
        case qs is
          when "01" => q <= (q(30 downto 0) & not alu(31));
          when "10" => q <= (alu(0) & q(31 downto 1));
          when "11" => q <= alu(31 downto 0);
			 when others => null;
        end case;
      end if;
	end if;
end process do_q;

-- page SHIFT0-1

with s(1 downto 0) select
	sa <= m when "00",
			m(30 downto 0) & m(31) when "01",
			m(29 downto 0) & m(31) & m(30) when "10",
			m(28 downto 0) & m(31) & m(30) & m(29) when others;


with s(4 downto 2) select
	rtmp(3 downto 0) <=
--	(r(12) & r(8) & r(4) & r(0)) <=
		(sa(12) & sa(8) & sa(4) & sa(0))		when "000",
		(sa(8) & sa(4) & sa(0) & sa(28)) 	when "001",
		(sa(4) & sa(0) & sa(28) & sa(24)) 	when "010",
		(sa(0) & sa(28) & sa(24) & sa(20)) 	when "011",
		(sa(28) & sa(24) & sa(20) & sa(16))	when "100",
		(sa(24) & sa(20) & sa(16) & sa(12))	when "101",
		(sa(20) & sa(16) & sa(12) & sa(8))	when "110",
		(sa(16) & sa(12) & sa(8) & sa(4))	when others;

r(12)	<= rtmp(3);
r(8)	<= rtmp(2);
r(4)	<= rtmp(1);
r(0)	<= rtmp(0);

with s(4 downto 2) select
	rtmp(7 downto 4) <=
--	(r(13) & r(9) & r(5) & r(1)) <=
		(sa(13) & sa(9) & sa(5) & sa(1))		when "000",
		(sa(9) & sa(5) & sa(1) & sa(29))		when "001",
		(sa(5) & sa(1) & sa(29) & sa(25)) 	when "010",
		(sa(1) & sa(29) & sa(25) & sa(21)) 	when "011",
		(sa(29) & sa(25) & sa(21) & sa(17)) when "100",
		(sa(25) & sa(21) & sa(17) & sa(13)) when "101",
		(sa(21) & sa(17) & sa(13) & sa(9)) 	when "110",
		(sa(17) & sa(13) & sa(9) & sa(5)) 	when others;

r(13) <= rtmp(7);
r(9)	<= rtmp(6);
r(5)	<= rtmp(5);
r(1)	<=	rtmp(4);

with s(4 downto 2) select
	rtmp(11 downto 8) <=
--	(r(14) & r(10) & r(6) & r(2)) <=
		(sa(14) & sa(10) & sa(6) & sa(2)) 	when "000",
		(sa(10) & sa(6) & sa(2) & sa(30)) 	when "001",
		(sa(6) & sa(2) & sa(30) & sa(26)) 	when "010",
		(sa(2) & sa(30) & sa(26) & sa(22)) 	when "011",
		(sa(30) & sa(26) & sa(22) & sa(18)) when "100",
		(sa(26) & sa(22) & sa(18) & sa(14)) when "101",
		(sa(22) & sa(18) & sa(14) & sa(10)) when "110",
		(sa(18) & sa(14) & sa(10) & sa(6)) 	when others;

r(14)	<= rtmp(11);
r(10)	<= rtmp(10);
r(6)	<= rtmp(9);
r(2)	<= rtmp(8);

with s(4 downto 2) select
	rtmp(15 downto 12) <=
--	(r(15) & r(11) & r(7) & r(3)) <=
		(sa(15) & sa(11) & sa(7) & sa(3)) 	when "000",
		(sa(11) & sa(7) &  sa(3) & sa(31)) 	when "001",
		(sa(7) & sa(3) & sa(31) & sa(27)) 	when "010",
		(sa(3) & sa(31) & sa(27) & sa(23)) 	when "011",
		(sa(31) & sa(27) & sa(23) & sa(19)) when "100",
		(sa(27) & sa(23) & sa(19) & sa(15)) when "101",
		(sa(23) & sa(19) & sa(15) & sa(11)) when "110",
		(sa(19) & sa(15) & sa(11) & sa(7)) 	when others;

r(15)	<= rtmp(15);
r(11)	<= rtmp(14);
r(7)	<= rtmp(13);
r(3)	<= rtmp(12);

--

with s(4 downto 2) select
	rtmp(19 downto 16) <=
--	(r(28) & r(24) & r(20) & r(16)) <=
		(sa(28) & sa(24) & sa(20) & sa(16))	when "000",
		(sa(24) & sa(20) & sa(16) & sa(12)) when "001",
		(sa(20) & sa(16) & sa(12) & sa(8))	when "010",
		(sa(16) & sa(12) & sa(8) & sa(4))	when "011",
		(sa(12) & sa(8) & sa(4) & sa(0))		when "100",
		(sa(8) & sa(4) & sa(0) & sa(28))		when "101",
		(sa(4) & sa(0) & sa(28) & sa(24))	when "110",
		(sa(0) & sa(28) & sa(24) & sa(20))	when others;

r(28)	<= rtmp(19);
r(24)	<= rtmp(18);
r(20)	<= rtmp(17);
r(16)	<= rtmp(16);

with s(4 downto 2) select
	rtmp(23 downto 20) <=
--	(r(29) & r(25) & r(21) & r(17)) <=
		(sa(29) & sa(25) & sa(21) & sa(17))	when "000",
		(sa(25) & sa(21) & sa(17) & sa(13))	when "001",
		(sa(21) & sa(17) & sa(13) & sa(9))	when "010",
		(sa(17) & sa(13) & sa(9) & sa(5))	when "011",
		(sa(13) & sa(9) & sa(5) & sa(1))		when "100",
		(sa(9) & sa(5) & sa(1) & sa(29))		when "101",
		(sa(5) & sa(1) & sa(29) & sa(25))	when "110",
		(sa(1) & sa(29) & sa(25) & sa(21))	when others;

r(29)	<= rtmp(23);
r(25)	<= rtmp(22);
r(21)	<= rtmp(21);
r(17)	<= rtmp(20);

with s(4 downto 2) select
	rtmp(27 downto 24) <=
--	(r(30) & r(26) & r(22) & r(18)) <=
		(sa(30) & sa(26) & sa(22) & sa(18))	when "000",
		(sa(26) & sa(22) & sa(18) & sa(14))	when "001",
		(sa(22) & sa(18) & sa(14) & sa(10))	when "010",
		(sa(18) & sa(14) & sa(10) & sa(6))	when "011",
		(sa(14) & sa(10) & sa(6) & sa(2))	when "100",
		(sa(10) & sa(6) & sa(2) & sa(30))	when "101",
		(sa(6) & sa(2) & sa(30) & sa(26))	when "110",
		(sa(2) & sa(30) & sa(26) & sa(22))	when others;

r(30)	<= rtmp(27);
r(26)	<= rtmp(26);
r(22)	<= rtmp(25);
r(18)	<= rtmp(24);

with s(4 downto 2) select
	rtmp(31 downto 28) <=
--	(r(31) & r(27) & r(23) & r(19)) <=
		(sa(31) & sa(27) & sa(23) & sa(19))	when "000",
		(sa(27) & sa(23) & sa(19) & sa(15))	when "001",
		(sa(23) & sa(19) & sa(15) & sa(11))	when "010",
		(sa(19) & sa(15) & sa(11) & sa(7))	when "011",
		(sa(15) & sa(11) & sa(7) & sa(3))	when "100",
		(sa(11) & sa(7) & sa(3) & sa(31))	when "101",
		(sa(7) & sa(3) & sa(31) & sa(27))	when "110",
		(sa(3) & sa(31) & sa(27) & sa(23))	when others;

r(31)	<= rtmp(31);
r(27)	<= rtmp(30);
r(23)	<= rtmp(29);
r(19)	<= rtmp(28);

-- page SMCTL

mr_n <= not (irbyte_n or ir(13));
sr_n <= not (irbyte_n or ir(12));

s(0) <= not (sr_n or not ir(0));
s(1) <= not (sr_n or not ir(1));


mskr(4) <= not (mr_n or sh4_n);
mskr(3) <= not (mr_n or sh3_n);
mskr(2) <= not (mr_n or not ir(2));
mskr(1) <= not (mr_n or not ir(1));
mskr(0) <= not (mr_n or not ir(0));


s(4) <= not (sr_n or sh4_n);
--s4_n <= sr_n or sh4_n;

s(3) <= not (sr_n or sh3_n);
s(2) <= not (sr_n or not ir(2));

mskl <= mskr + ir(9 downto 5);

-- page SOURCE

irdisp <= not irdisp_n;
irjump <= not irjump_n;

--(irbyte_n & irdisp_n & irjump_n & iralu_n) <=
irflags <=
			"1111" when nop='1' else
			"1110" when ir(44)='0' and ir(43)='0' else
			"1101" when ir(44)='0' and ir(43)='1' else
			"1011" when ir(44)='1' and ir(43)='0' else
			"0111";

irbyte_n	<= irflags(3);
irdisp_n	<= irflags(2);
irjump_n	<= irflags(1);
iralu_n	<= irflags(0);

funct <= "0000" when nop='1' else
			"0001" when ir(11)='0' and ir(10)='0' else
			"0010" when ir(11)='0' and ir(10)='1' else
			"0100" when ir(11)='1' and ir(10)='0' else
			"1000";

iralu <= not iralu_n;
funct2_n <= not funct(2);

specalu_n <= not (ir(8) and iralu);


divmul <= "11" when specalu_n='1' else
									"10" when ir(4)='0' and ir(3)='0' else
									"01";
div_n <= divmul(1);
mul_n <= divmul(0);

--xxx eliminate?
srclc <= not srclc_n;

srcflags(12 downto 5) <=
--(srcq_n & srcopc_n & srcpdltop_n & srcpdlpop_n &
--	srcpdlidx_n & srcpdlptr_n & srcspc_n & srcdc_n) <=
					"11111111" when ir(31)='0' or ir(29)='0' else
					"11111110" when ir(28)='0' and ir(27)='0' and ir(26)='0' else
					"11111101" when ir(28)='0' and ir(27)='0' and ir(26)='1' else
					"11111011" when ir(28)='0' and ir(27)='1' and ir(26)='0' else
					"11110111" when ir(28)='0' and ir(27)='1' and ir(26)='1' else
					"11101111" when ir(28)='1' and ir(27)='0' and ir(26)='0' else
					"11011111" when ir(28)='1' and ir(27)='0' and ir(26)='1' else
					"10111111" when ir(28)='1' and ir(27)='1' and ir(26)='0' else
	            "01111111";

srcq_n		<= srcflags(12);
srcopc_n		<= srcflags(11);
srcpdltop_n	<= srcflags(10);
srcpdlpop_n	<= srcflags(9);
srcpdlidx_n	<= srcflags(8);
srcpdlptr_n	<= srcflags(7);
srcspc_n		<= srcflags(6);
srcdc_n		<= srcflags(5);

srcflags(4 downto 0) <=
--(srcspcpop_n & srclc_n & srcmd_n & srcmap_n & srcvma_n) <=
					"11111" when ir(31)='0' or ir(29)='0' else
					"11110" when ir(28)='0' and ir(27)='0' and ir(26)='0' else
					"11101" when ir(28)='0' and ir(27)='0' and ir(26)='1' else
					"11011" when ir(28)='0' and ir(27)='1' and ir(26)='0' else
					"10111" when ir(28)='0' and ir(27)='1' and ir(26)='1' else
					"01111" when ir(28)='1' and ir(27)='0' and ir(26)='0' else
	            "11111";

srcspcpop_n	<= srcflags(4);
srclc_n		<= srcflags(3);
srcmd_n		<= srcflags(2);
srcmap_n		<= srcflags(1);
srcvma_n		<= srcflags(0);

imod <= not ((destimod0_n and iwrited_n) and destimod1_n and idebug_n);

destmem_n <= not (destm and ir(23));
destvma_n <= destmem_n or ir(22);
destmdr_n <= destmem_n or not ir(22);

dest <= not (iralu_n and irbyte_n);
destm <= dest and not ir(25);

destflags(9 downto 8) <=
--(destintctl_n & destlc_n) <=
			"11" when not(destm='1' and ir(23)='0' and ir(22)='0') else
			"10" when ir(21)='0' and ir(20)='0' and ir(19)='1' else
			"01" when ir(21)='0' and ir(20)='1' and ir(19)='0' else
			"11";

destintctl_n	<= destflags(9);
destlc_n			<= destflags(8);

--(destimod1_n & destimod0_n & destspc_n & destpdlp_n &
--	destpdlx_n & destpdl_x_n & destpdl_p_n & destpdltop_n) <=
destflags(7 downto 0) <=
			"11111111" when not (destm='1' and ir(23)='0' and ir(22)='1') else
			"11111110" when ir(21)='0' and ir(20)='0' and ir(19)='0' else
			"11111101" when ir(21)='0' and ir(20)='0' and ir(19)='1' else
			"11111011" when ir(21)='0' and ir(20)='1' and ir(19)='0' else
			"11110111" when ir(21)='0' and ir(20)='1' and ir(19)='1' else
			"11101111" when ir(21)='1' and ir(20)='0' and ir(19)='0' else
			"11011111" when ir(21)='1' and ir(20)='0' and ir(19)='1' else
			"10111111" when ir(21)='1' and ir(20)='1' and ir(19)='0' else
			"01111111";

destimod1_n		<= destflags(7);
destimod0_n		<= destflags(6);
destspc_n		<= destflags(5);
destpdlp_n		<= destflags(4);
destpdlx_n		<= destflags(3);
destpdl_x_n		<= destflags(2);
destpdl_p_n		<= destflags(1);
destpdltop_n	<= destflags(0);

destspc <= not destspc_n;

-- page SPC

--part_32x19ram
i_spc : cadr2_spc port map (spcptr, spcw, spco, swp, clk);

--always @(posedge CLK)
do_spc: process(qclk)
begin
	if qclk'event and qclk='1' then
		if spcnt_n='0' then
			if spush_n='0' then
				spcptr <= spcptr + 1;
			else
				spcptr <= spcptr - 1;
			end if;
		end if;
	end if;
end process do_spc;

-- page SPCLCH

-- mux SPC
spc <= spco_latched when spcpass_n='0' else
			spcw when spcwpass_n='0' else
			"0000000000000000000";

spcopar <= '0';

-- transparent latch
--always @(CLK or spco or spcopar or negedge reset_n)
do_spclch: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			spco_latched <= "0000000000000000000";
		else
			spco_latched <= spco;
			spcpar <= spcopar;
      end if;
	end if;
end process do_spclch;

-- page SPCPAR

spcwpar <= '0';
spcwparl_n <= '1';
spcwparh <= '0';
spcparok <= '1';

spcwpar <= spcwparh xor spcwparl_n;

-- page SPCW

do_spcw: process(CLK)
begin
	if clk'event and clk='1' then
		if reset = '1' then
			reta <= "00000000000000";
		elsif n='1' then
			reta <= wpc;
		else
			reta <= ipc;
		end if;
	end if;
end process do_spcw;

spcw <= l(18 downto 0) when destspcd='1' else ("00000" & reta);

-- page SPY1-2

--xxxdebug
--assign spy = 16'b1111111111111111;

--spy <= ir(47 downto 32) when spy_irh_n='0',
--			ir(31 downto 16) when spy_irm_n='0',
--			ir(15 downto 0) when spy_irl_n='0',
--			ob(31 downto 16) when spy_obh_n='0',
--			ob(15 downto 0) when spy_obl_n='0',
--			a(31 downto 16) when spy_ah_n='0',
--			a(15 downto 0) when spy_al_n='0',
--			m(31 downto 16) when spy_mh_n='0'
--			m(15 downto 0) when spy_ml_n='0',
--			("00" & wmapd & destspcd & iwrited & imodd & pdlwrited & spushd &
--			 "00" & ir(48) & nop & vmaok_n & jcond & pcs1 & pcs0) when spy_flag2_n='0',
--			("00" & opc) when spy_opc_n='0',
--			(wait_n & v1pe_n & v0pe_n & promdisable & stathalt_n & err & ssdone & srun &
--			 higherr_n & mempe_n & ipe_n & dpe_n &
--			 spe_n & pdlpe_n & mpe_n & ape_n) when spy_flag1_n='0',
--			("00" & pc) when spy_pc_n='0',
--			X"FFFF" when others;

halt_n <= '1';

-- page TRAP

mdpareven <= '0';

mdparerr <= mdpareven xor mdpar;
parerr_n <= not (mdparerr and mdhaspar and use_md and wait_n);
memparok <= not memparok_n;

trap_n <= not(not(parerr_n or not trapenb) or boot_trap);
trap <= not trap_n;
memparok_n <= not(parerr_n or trapenb);

-- page VCTRL1

memop_n <= memrd_n and memwr_n and ifetch_n;
memprepare <= not(memop_n or clk);

do_memstart: process(mclk)
begin
	if mclk'event and mclk='1' then
		if reset_n='0' then
			memstart <= '0';
			mbusy_sync <= '0';
		else
			memstart <= memprepare;
			mbusy_sync <= memrq;
		end if;
	end if;
end process do_memstart;

memstart_n <= not memstart;

pfw_n <= not (lvmo_n(22) and wrcyc);
vmaok_n <= not(pfr_n and pfw_n);

do_wmapd: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			wrcyc <= '0';
			wmapd <= '0';
		else
			wrcyc <= not((memprepare and memwr_n) or (not memprepare and rdcyc));
			wmapd <= wmap;
		end if;
	end if;
end process do_wmapd;

rdcyc <= not wrcyc;
wmapd_n <= not wmapd;

memrq <= mbusy or (memstart and pfr_n and pfw_n);

--------
do_mbusy: process(MCLK)
 begin
	if MCLK'event and MCLK='1' then
		if mfinishd_n='0' then
			mbusy <= '0';
		else
			mbusy <= memrq;
		end if;
	end if;
end process do_mbusy;

--always @(posedge MCLK or negedge reset_n)
--  if (reset_n == 0)
--    mbusy <= 1'b0;
--  else
--    mbusy <= memrq;
--
--always @(mfinishd_n)
--  if (mfinishd_n == 1'b0)
--      mbusy <= 1'b0;

--------

set_rd_in_progess <= rd_in_progress or (memstart and pfr_n and rdcyc);
mfinish_n <= memack_n and reset_n;

do_rd_in_progress: process(MCLK)
begin
	if MCLK'event and MCLK='1' then
		if rdfinish_n='0' then
			rd_in_progress <= '0';
		else
			rd_in_progress <= set_rd_in_progess;
		end if;
	end if;
end process do_rd_in_progress;

--XXX delay line
-- mfinish_n + 30ns -> mfinishd_n
-- mfinish_n + 140ns -> rdfinish_n

do_mcycle_delay: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			mcycle_delay <= "1111111111";
		else
			mcycle_delay(0) <= mfinish_n;
			mcycle_delay(1) <= mcycle_delay(0);
			mcycle_delay(2) <= mcycle_delay(1);
			mcycle_delay(3) <= mcycle_delay(2);
			mcycle_delay(4) <= mcycle_delay(3);
			mcycle_delay(5) <= mcycle_delay(4);
			mcycle_delay(6) <= mcycle_delay(5);
			mcycle_delay(7) <= mcycle_delay(6);
		end if;
	end if;
end process do_mcycle_delay;

mfinishd_n <= mcycle_delay(2);
rdfinish_n <= mcycle_delay(7);

wait_n <= not(
	(not destmem_n and mbusy_sync) or
	(use_md and mbusy and memgrant_n) or		-- hang loses
	(lcinc and needfetch and mbusy_sync)		-- ifetch
	);

hang_n <= not (rd_in_progress and use_md and not clk);

-- page VCTRL2

mapwr0d <= not (wmapd_n or not vma(26));
mapwr1d <= not (wmapd_n or not vma(25));

vm0wp_n <= not (mapwr0d and wp);
vm1wp <= mapwr1d and wp;

vmaenb_n <= destvma_n and ifetch_n;
vmasel <= ifetch_n and '1';

-- external?
lm_drive_enb <= '0';

memdrive_n <= not (wrcyc and lm_drive_enb);

mdsel <= not (destmdr_n or clk);

use_md <= not (srcmd_n or nopa);

pfr_n <= not lvmo_n(23);

memflags <=
--(wmap_n & memwr_n & memrd_n) <=
			"111" when destmem_n='1' else
			"110" when ir(20)='0' and ir(19)='1' else
			"101" when ir(20)='1' and ir(19)='0' else
			"011" when ir(20)='1' and ir(19)='1' else
			"111";
			
wmap_n	<= memflags(2);
memwr_n	<= memflags(1);
memrd_n	<= memflags(0);

wmap <= not wmap_n;

-- page VMA

do_vma: process(clk)
begin
	if clk'event and clk='1' then
		if reset_n='0' then
			vma <= X"00000000";
		elsif vmaenb_n='0' then
			vma <= vmas;
		end if;
	end if;
end process do_vma;

vmadrive_n <= not (not srcvma_n and tse);

-- page VMAS

vmas <= ob when vmasel='1' else ("00000000" & lc(25 downto 2));

mapi <= md(23 downto 8) when memstart_n='1' else vma(23 downto 8);

-- page VMEM0 - virtual memory map stage 0

--part_2kx5ram
i_vmem0: cadr2_vmem0 port map (mapi(23 downto 13), vma(31 downto 27), vmap_n, vm0wp_n, '1', clk);

srcmap <= not srcmap_n;
use_map_n <= not (srcmap or memstart);
vmoparck <= use_map_n or vmoparodd;
v0parok <= use_map_n or '1';
vm0pari <= '0';

vmopar <= '0';

vmoparodd <= vmopar xor vmoparck;

-- page VMEM1&2

mapi_n <= not mapi(12 downto 8);

vmo <= not vmo_n;

vmap <= not vmap_n;

vmem1_adr <= (mapi_n(12 downto 8) & vmap(4 downto 0));

--part_1kx24ram
i_vmem1_2: cadr2_vmem1 port map (vmem1_adr, vma(23 downto 0), vmo_n, vm1wp, '1', clk);

vm1mpar <= '0';
vm1lpar <= '0';
vm0par <= '0';
vm0parm <= '0';
vm0parl <= '0';

-- page VMEMDR - map output drive

-- transparent latch
lvmo_n <= vmo_n(23 downto 22) when memstart='1';
pma <= vmo_n(21 downto 8) when memstart='1';

mapdrive_n <= not(tse and srcmap);

-- page DEBUG

--spy_ir_h: process(lddbirh_n)
--begin
--	if lddbirh_n'event and lddbirh_n='1' then
--     spy_ir(47 downto 32) <= spy;
--	end if;
--end process spy_ir_h;

--spy_ir_m: process(lddbirm_n)
--begin
--	if lddbirm_n'event and lddbirm_n='1' then
--     spy_ir(31 downto 16) <= spy;
--	end if;
--end process spy_ir_m;

--spy_ir_l: process(lddbirl_n)
--begin
--	if lddbirl_n'event and lddbirl_n='1' then
--     spy_ir(15 downto 0) <= spy;
--	end if;
--end process spy_ir_l;

-- put latched value on I bus when idebug_n asserted
--i <= spy_ir when idebug_n='0' else iram;
i <= iram;


-- page ICTL - I RAM control

promdisabled_n <= not promdisabled;

ramdisable <= idebug or (promdisabled_n and iwrited_n);

-- see clocks below
--assign iwe  = !(wp5& iwriteda);

-- page OLORD1 

--do_olord1: process(ldmode_n)
--begin
--	if ldmode_n'event and ldmode_n='1' then
--		if reset_n='0' then
--			promdisable <= 0;
--			trapenb <= 0;
--			stathenb <= 0;
--			errstop <= 0;
--			speed1 <= 0;
--			speed0 <= 0;
--		else
--			promdisable <= spy(5);
--			trapenb <= spy(4);
--			stathenb <= spy(3);
--			errstop <= spy(2);
--			speed1 <= spy(1);
--			speed0 <= spy(0);
--		end if;
--	 end if;
--end process do_olord1;

--ldopc <= not ldopc_n;

--do_olord1a: process(ldopc)
--begin
--	if ldopc'event and ldopc='1' then
--		if reset_n='0' then
--			opcinh <= 0;
--			opcclk <= 0;
--			lpc_hold <= 0;
--		else
--			opcinh <= spy(2);
--			opcclk <= spy(1);
--			lpc_hold <= spy(0);
--		end if;
--	end if;
--end process do_olord1a;

--opcinh_n <= not opcinh;
--opcclk_n <= not opcclk;

--ldclk <= not ldclk_n;

--do_olord1b: process(ldclk)
--begin
--	if ldclk'event and ldclk='1' then
--		if reset_n='0' then
--			ldstat <= 0;
--			idebug <= '0';
--			nop11 <= '0';
--			step <= '0';
--		else
--			ldstat <= spy(4);
--			idebug <= spy(3);
--			nop11 <= spy(2);
--			step <= spy(1);
--		end if;
--	end if;
--end process do_olord1b;


ldstat_n <= not ldstat;
idebug_n <= not idebug;
nop11_n <= not nop11;
step_n <= not step;

--always @(posedge ldclk_n or negedge clock_reset_n or negedge boot_n)
--  if (boot_n == 1'b0)
--    run <= 1'b1;
--  else
--    if (clock_reset_n == 1'b0)
--      run <= 1'b0;
--    else
--     run <= spy[0];

run <= '1';

do_clk_reset: process(MCLK)
begin
	if MCLK'event and MCLK='1' then
		if clock_reset_n='0' then
			srun <= '0';
			sstep <= '0';
			ssdone <= '0';
			promdisabled <= '0';
		else
			srun <= run;
			sstep <= step;
			ssdone <= sstep;
			promdisabled <= promdisable;
		end if;
	end if;
end process do_clk_reset;


--xxx delay line
--assign speedclk = !(tpr60_n);

--always @(posedge speedclk)
--  begin
--    speed0a <= speed0;
--    speed1a <= speed1;
--    sspeed0 <= speed0a;
--    sspeed1 <= speed1a;
--  end
--
--always @(clock_reset_n)
--  if (clock_reset_n == 0)
--    begin
--      speed0a = 0;
--      speed1a = 0;
--      sspeed0 = 0;
--      sspeed1 = 0;
--    end

--initial
--    begin
--      speed0a = 0;
--      speed1a = 0;
--      sspeed0 = 0;
--      sspeed1 = 0;
--    end

ssdone_n <= not ssdone;

machrun <= (sstep and ssdone_n) or (srun and errhalt_n and
		wait_n and stathalt_n);

--assign stat_ovf = ~stc32;
--assign stat_ovf = 0'b0;
stathalt_n <= not (statstop and stathenb);

-- page OLORD2

spcoparok <= '1';
vm0parok <= '1';
pdlparok <= '1';

do_olord2: process(clk)
begin
	if clk'event and clk='1' then
		ape_n <= aparok;
		mpe_n <= mmemparok;
		pdlpe_n <= pdlparok;
		dpe_n <= dparok;
		ipe_n <= iparok;
		spe_n <= spcparok;
		higherr_n <= highok;
		mempe_n <= memparok;

		v0pe_n <= v0parok;
		v1pe_n <= vm0parok;
		statstop <= stat_ovf;
		halted_n <= halt_n;
	end if;
end process do_olord2;

lowerhighok_n <= '0';
highok <= '1';
--ldmode <= not ldmode_n;

--prog_reset_n <= not (ldmode and spy(6));

--reset <= not (boot_n and clock_reset_n and prog_reset_n);
reset_n <= not reset;

err <= not ape_n or not mpe_n or not pdlpe_n or not dpe_n or
	not ipe_n or not spe_n or not higherr_n or not mempe_n or
	not v0pe_n or not v1pe_n or not halted_n;

errhalt_n <= not (errstop and err);

-- external
prog_bus_reset <= '0';

bus_reset_n <= not (prog_bus_reset or power_reset);
bus_power_reset_n <= not power_reset;

--external power_reset_n - low by rc, external input
--power_reset <= not power_reset_n;

-- external
busint_lm_reset_n <= '1';

clock_reset_n <= not (power_reset or not busint_lm_reset_n);

--prog_boot <= ldmode and spy(7);

--boot_n <= not(prog_boot);

do_boot_trap: process(clk)
begin
	if clk'event and clk='1' then
		if clock_reset_n='0' then
			boot_trap <= '0';
--		elsif boot_n='0' then
--			boot_trap <= '1';
		elsif srun='1' then
			boot_trap <= '0';
		end if;
	end if;
end process do_boot_trap;

-- page OPCS

opcclka <= not(not CLK or opcclk);

opc_inh_or_clka <= opcinh or opcclka;

do_opc: process(opc_inh_or_clka)
begin
	if opc_inh_or_clka'event and opc_inh_or_clka='1' then
		opc <= pc;
	end if;
end process do_opc;


-- With the machine stopped, taking OPCCLK high then low will
-- generate a clock to just the OPCS.
-- Setting OPCINH high will prevent the OPCS from clocking when
-- the machine runs.  Only change OPCINH when CLK is high 
-- (e.g. machine stopped).

-- page IRAM

i_iram: cadr2_wcs port map(pc, iwr, iram, iwe, '1', clk);

-- page SPY0

-- read registers
--(spy_obh_n & spy_obl_n & spy_pc_n & spy_opc_n &
--	spy_nc_n & spy_irh_n & spy_irm_n & spy_irl_n) <=
--			"11111111" when eadr(3)='1' and dbread_n='1',
--			"11111110" when eadr(2)='0' and eadr(1)='0' and eadr(0)='0',
--			"11111101" when eadr(2)='0' and eadr(1)='0' and eadr(0)='1',
--			"11111011" when eadr(2)='0' and eadr(1)='1' and eadr(0)='0',
--			"11110111" when eadr(2)='0' and eadr(1)='1' and eadr(0)='1',
--			"11101111" when eadr(2)='1' and eadr(1)='0' and eadr(0)='0',
--			"11011111" when eadr(2)='1' and eadr(1)='0' and eadr(0)='1',
--			"10111111" when eadr(2)='1' and eadr(1)='1' and eadr(0)='0',
--			"01111111" when others;

-- read registers
--(spy_sth_n & spy_stl_n & spy_ah_n & spy_al_n &
--	spy_mh_n & spy_ml_n & spy_flag2_n & spy_flag1_n) <=
--			"11111111" when eadr(3)='0' and dbread_n='1',
--			"11111110" when eadr(2)='0' and eadr(1)='0' and eadr(0)='0',
--			"11111101" when eadr(2)='0' and eadr(1)='0' and eadr(0)='1',
--			"11111011" when eadr(2)='0' and eadr(1)='1' and eadr(0)='0',
--			"11110111" when eadr(2)='0' and eadr(1)='1' and eadr(0)='1',
--			"11101111" when eadr(2)='1' and eadr(1)='0' and eadr(0)='0',
--			"11011111" when eadr(2)='1' and eadr(1)='0' and eadr(0)='1',
--			"10111111" when eadr(2)='1' and eadr(1)='1' and eadr(0)='0',
--	      "01111111" when others;

-- load registers
--(ldmode_n & ldopc_n & ldclk_n & lddbirh_n & lddbirm_n & lddbirl_n) <=
--			"111111" when dbwrite_n='1',
--			"111110" when eadr(2)='0' and eadr(1)='0' and eadr(0)='0',
--			"111101" when eadr(2)='0' and eadr(1)='0' and eadr(1)='0',
--			"111011" when eadr(2)='0' and eadr(1)='1' and eadr(0)='0',
--			"110111" when eadr(2)='0' and eadr(1)='1' and eadr(0)='1',
--			"101111" when eadr(2)='1' and eadr(1)='0' and eadr(0)='0',
--			"011111" when eadr(2)='1' and eadr(1)='0' and eadr(0)='1',
--			"111111" when others;

-- *******
-- Clocks
-- This circuitry is lifted from the LM-2, which replaced the delay
-- lines with a clock chain.
-- *******

--xxx need to clean these up
mclk0_n <= tpclk_n;

MCLK <= not mclk0_n;
--assign CLK = lclk_n;
--CLK <= lclk;

--assign clk0_n = tpclk_n & machrun;

-- LM-2 clocks

-- -------

clk_n <= tpclk_n and machrun;

lclk <= not clk_n ;
lclk_n <= not lclk ;
ltse_n <= not tptse ;
tse <= not ltse_n ;
lwp_n <= not tpwp ;
wp <= not lwp_n ;

tpr0a <= not tpr0_n ;
tpr0d_n <= not tpr0a;
tpr1a <= not tpr1_n ;
tpr1d_n <= not tpr1a ;

tpwp <= tpw1 and crbs_n and machrun;
-- ?? tpwpor1
tpwpiram <= tpwpor1 and crbs_n and machrun;

tptsef <= not (tpr1d_n and crbs_n and tptsef_n);
iwe <= (iwrited and tpwpiram);
tpclk_n <= not (tpw0_n and crbs_n and tpclk);

do_clocks1: process(hifreq1)
begin
	if hifreq1'event and hifreq1='1' then
		tpr6_n <= tpr5;
		tpw3 <= tpw2;
		tpw3_n <= not tpw2;
		tpw2 <= tpw1;
		tpw2_n <= not tpw1;
		tpw1 <= tpw0;
		tpw1_n <= not tpw0;

		tpr5 <= tpr4;
		tpr5_n <= not tpr4;

		tpr4 <= tpr3;
		tpr4_n <= not tpr3;

		tpr3 <= tpr2;
		tpr3_n <= not tpr2;

		tpr2_n <= sone_n ;
		tpr2 <= not sone_n ;
	end if;
end process do_clocks1;

maskc <= not (tendly_n and tpr1_n);
ff1_n <= not (tpr1_n and ff1);
tpclk <= not (tpclk_n and tpr0_n);
tptsef_n <= not (tpr0d_n and tptsef);

tpwpor1 <= not (tpw0_n and tpw1_n);
tptse <= not (tptsef_n and machrun);

osc0 <= not clk;
hifreq1 <= not osc0;
hifreq2 <= not osc0;
hf_n <= not hifreq2;
hfdlyd <= not hf_n;
hftomm <= not hfdlyd;

do_clocks2: process(hifreq1)
begin
	if hifreq1'event and hifreq1='1' then
		ff1d <= ff1;
		tpr1_n <= tpr0_n;
		tpr1 <= not tpr0_n;
	end if;
end process do_clocks2;

ff1 <= not (tpw2_n and crbs_n and ff1_n);
tpr0_n <= not(ff1d and hangs_n and crbs_n);
sone_n <= not(tpr1 and tpr3_n and tpr4_n);

tprend_n <=
		not tpr6_n when sspeed1a='0' and sspeed0a='0' and ilong_n='0' else
		not tpr5_n when sspeed1a='0' and sspeed0a='0' and ilong_n='1' else
		not tpr5_n when sspeed1a='0' and sspeed0a='1' and ilong_n='0' else
		not tpr4_n when sspeed1a='0' and sspeed0a='1' and ilong_n='1' else
		not tpr4_n when sspeed1a='1' and sspeed0a='0' and ilong_n='0' else
		not tpr3_n when sspeed1a='1' and sspeed0a='0' and ilong_n='1' else
		not tpr4_n when sspeed1a='1' and sspeed0a='1' and ilong_n='0' else
		not tpr2_n;

do_clocks3: process(tpr1)
begin
	if tpr1'event and tpr1='1' then
		if clock_reset_n='0' then
			speed1a <= '0';
			speed0a <= '0';
			sspeed1a <= '0';
			sspeed0a <= '0';
		else
			speed1a <= speed1;
			speed0a <= speed0;
			sspeed1a <= speed1a;
			sspeed0a <= speed0a;
		end if;
	end if;
end process do_clocks3;

do_clocks4: process(hfdlyd)
begin
	if hfdlyd'event and hfdlyd='1' then
		tendly_n <= tprend_n;
	end if;
end process do_clocks4;

do_clocks5: process(hifreq2)
begin
	if hifreq2'event and hifreq2='1' then
		tpw0_n <= maskc;
		tpw0 <= not maskc;

		hangs_n <= hang_n;
		crbs_n <= clock_reset_n ;
	end if;
end process do_clocks5;


-- *******
-- Resets!
-- *******


--external
--assign memack_n = 1;
--assign memgrant_n = 1;
--assign mempar_in = 0;
--assign adrpar_n = 0;
--assign loadmd = 0;
--assign ignpar_n = 1;


-- traditional CADR signals to xbus
-- mem[31:0]
-- mempar_in
-- adrpar_n
-- (pma[21:0],vma[7:0])
-- memrq_n
-- memack_n
-- loadmd_n
-- ignpar_n
-- memgrant_n
-- wrcyc
-- int
-- mempar_out

--wire bus_int;

--  busint busint(
--	.bus(busint_bus),
--	.addr((pma,vma[7:0])),
--	.spy(spy),
--	.mclk(MCLK),
--	.mempar_in(mempar_in),
--	.adrpar_n(adrpar_n),
--	.req(memrq),
--	.ack_n(memack_n),
--	.loadmd(loadmd),
--	.ignpar(ignpar_n),
--	.memgrant_n(memgrant_n),
--	.wrcyc(wrcyc),
--	.int(bus_int),
--	.mempar_out(mempar_out),
--	.reset_n(reset_n)
--	);

end Behavioral;

