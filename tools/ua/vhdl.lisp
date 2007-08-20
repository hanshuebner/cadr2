;;;; -*- Mode: LISP; Package: UA -*-
;;;;     MICRO ASSEMBLER  FOR CADR

;;; 

(in-package "UA")

(defun write-vhdl-files (file)
  (write-i-mem file)
  (write-d-mem file))

(defun write-i-mem (file)
  (with-open-file (out-file (merge-pathnames #p"cadr2_wcs.vhd")
			    :direction :output
			    :if-exists :supersede)
	(format out-file "--  cadr2_wcs.vhd~%~%")
	;; More VHDL comments needed here
	(format out-file "library IEEE;~%")
	(format out-file "use IEEE.STD_LOGIC_1164.ALL;~%")
	(format out-file "use IEEE.STD_LOGIC_ARITH.ALL;~%")
	(format out-file "use IEEE.STD_LOGIC_UNSIGNED.ALL;~%~%")
	(format out-file "library UNISIM;~%")
	(format out-file "use UNISIM.VComponents.all;~%~%")
	(format out-file "entity cadr2_wcs is~%")
	(format out-file "  Port (addr : in std_logic_vector(13 downto 0);~%")
	(format out-file "        di   : in std_logic_vector(48 downto 0);~%")
	(format out-file "        do   : out std_logic_vector(48 downto 0);~%")
	(format out-file "        wr   : in std_logic;~%")
	(format out-file "        en   : in std_logic;~%")
	(format out-file "        clk  : in std_logic);~%end cadr2_wcs;~%~%")
	(format out-file "architecture low_level of cadr2_wcs is~%~%")
	(format out-file "signal dip : std_logic_vector(1 downto 0);~%")
	(format out-file "signal dop : std_logic_vector(1 downto 0);~%~%")
	(format out-file "begin~%~%")

	(write-bram-s18 i-mem 0 0 out-file)
	(write-bram-s18 i-mem 0 1 out-file)
	(write-bram-s18 i-mem 0 2 out-file)

	(format out-file "dip <= \"0\" & di(48);~%")
	(format out-file "do(48) <= dop(0);~%~%")
	(format out-file "end low_level;~%")))

(defun write-bram-s18 (mem slice block out-file)
  (format out-file "RAMB16_S18_~D_~D : RAMB16_S18~%" slice block)
  (format out-file "generic map (~%~TINIT => X\"00000\",~%")
  (format out-file "~TSRVAL => X\"00000\",~%")
  (format out-file "~TWRITE_MODE => \"WRITE_FIRST\"")

  (do ((start (* slice 1024))
       (i 0 (1+ i)))
      ((= i 64))
      (format out-file ",~%~TINIT_~2,'0X => X\""  i)
      (do ((j 16 (1- j)))
	  ((= j 0))
	  (format out-file "~4,'0X"
		  (ldb (byte 16 (* block 16))
		       (aref mem (+ start (* i 16) (1- j))))))
      (format out-file "\""))

  (do ((start (* slice 1024))
       (i 0 (1+ i)))
      ((= i 8))
      (format out-file ",~%~T~%INITP_0~X => X\""  i)
      (do ((j 64 (1- j)))
	  ((= j 0))
	  (format out-file "~X"
		  (+ (ldb (byte 1 48)
			  (aref mem (+ start (* i 128) (1- j))))
		     (ash (ldb (byte 1 48)
			       (aref mem (+ start (* i 128) (1- j) 1))) 2))))
      (format out-file "\""))
       
  (format out-file ")~%port map (~%")
  (format out-file "   DO => do(~D downto ~D),~%"
	  (+ (* block 16) 15) (* block 16))
  (if (= block 2)
      (format out-file "   DOP => dop,~%"))
  (format out-file "   ADDR => addr(9 downto 0),~%")
  (format out-file "   CLK => clk,~%")
  (format out-file "   DI => di(~D downto ~D),~%"
	  (+ (* block 16) 15) (* block 16))
  (format out-file "   DIP => dip,~%")
  (format out-file "   EN => en,~%")
  (format out-file "   SSR => '0',~%")
  (format out-file "   WE => wr);~%~%"))

(defun write-d-mem (file)
  (with-open-file (out-file (merge-pathnames #p"cadr2_dispatch.vhd")
			    :direction :output
			    :if-exists :supersede)
	(format out-file "--  cadr2_dispatch.vhd~%~%")
	;; More VHDL comments needed here
	(format out-file "library IEEE;~%")
	(format out-file "use IEEE.STD_LOGIC_1164.ALL;~%")
	(format out-file "use IEEE.STD_LOGIC_ARITH.ALL;~%")
	(format out-file "use IEEE.STD_LOGIC_UNSIGNED.ALL;~%~%")
	(format out-file "library UNISIM;~%")
	(format out-file "use UNISIM.VComponents.all;~%~%")
	(format out-file "entity cadr2_dispatch is~%")
	(format out-file "  Port (a    : in std_logic_vector(10 downto 0);~%")
	(format out-file "        di   : in std_logic_vector(16 downto 0);~%")
	(format out-file "        do   : out std_logic_vector(16 downto 0);~%")
	(format out-file "        wr   : in std_logic;~%")
	(format out-file "        en   : in std_logic;~%")
	(format out-file "        clk  : in std_logic);~%end cadr2_dispatch;~%~%")
	(format out-file "architecture low_level of cadr2_dispatch is~%~%")
	(format out-file "signal dip : std_logic_vector(0 downto 0);~%~%")
	(format out-file "begin~%~%")

	(write-bram-s9 d-mem 0 out-file)
	(write-bram-s9 d-mem 1 out-file)

	(format out-file "dip <= \"0\";~%~%")
	(format out-file "end low_level;~%")))

(defun write-bram-s9 (mem block out-file)
  (format out-file "RAMB16_S9_~D : RAMB16_S9~%" block)
  (format out-file "generic map (~%~TINIT => X\"000\",~%")
  (format out-file "~TSRVAL => X\"000\",~%")
  (format out-file "~TWRITE_MODE => \"WRITE_FIRST\"")

  (do ((i 0 (1+ i)))
      ((= i 64))
      (format out-file ",~%~TINIT_~2,'0X => X\""  i)
      (do ((j 32 (1- j)))
	  ((= j 0))
	  (format t "Element ~D~%" (+ (* i 32) (1- j)))
	  (format out-file "~2,'0X"
		  (ldb (byte 8 (* block 8))
		       (aref mem (+ (* i 32) (1- j))))))
      (format out-file "\""))

  (do ((i 0 (1+ i)))
      ((= i 8))
      (format out-file ",~%~T~%INITP_0~X => X\""  i)
      (do ((j 64 (1- j)))
	  ((= j 0))
	  (format out-file "~X"
		  (+ (ldb (byte 1 16)
			  (aref mem (+ (* i 128) (1- j))))
		     (ash (ldb (byte 1 16)
			       (aref mem (+ (* i 128) (1- j) 1))) 1))))
      (format out-file "\""))
       
  (format out-file ")~%port map (~%")
  (format out-file "   DO => do(~D downto ~D),~%"
	  (+ (* block 8) 7) (* block 8))
  (if (= block 1)
      (format out-file "   DOP => do(16 downto 16),~%"))
  (format out-file "   ADDR => a,~%")
  (format out-file "   CLK => clk,~%")
  (format out-file "   DI => di(~D downto ~D),~%"
	  (+ (* block 8) 7) (* block 8))
  (if (= block 1)
      (format out-file "   DIP => di(16 downto 16),~%")
    (format out-file "   DIP => dip,~%"))
  (format out-file "   EN => en,~%")
  (format out-file "   SSR => '0',~%")
  (format out-file "   WE => wr);~%~%"))

