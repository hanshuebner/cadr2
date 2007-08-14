;;; CADR PROM Microcode.			-*-Fundamental-*-

;;; There are many places in this code in which cycles are wasted.
;;; This is to make the code clearer, since it is far more important
;;; that code in a PROM be bug-free than that the boot sequence be
;;; 150 nanoseconds faster.

;;; Still to be done is the feature for loading microcode off the Chaos net.

(progn (print '(type t if for prom nil for ram))
       (setq prom (read))
(defvar promh '(

(locality m-mem)

m-garbage	(0)
m-hunoz		(0)
m-zero		(0)
m-ones		(0)

m-a		(0)
m-b		(0)
m-c		(0)
m-d		(0)

m-temp-1	(0)
m-temp-2	(0)

m-micr-offset	(0)

(locality a-mem)

a-garbage	(0)
a-hunoz		(0)
a-zero		(0)
a-ones		(0)

a-a		(0)
a-b		(0)
a-c		(0)
a-d		(0)

a-temp-1	(0)
a-temp-2	(0)

a-micr-offset	(0)

(loc 40)

;;; Constants.
a-1		(0)
a-2		(0)
a-3		(0)
a-4		(0)
a-5		(0)
a-11		(0)
a-40		(0)
a-400		(0)
a-20000		(0)
a-3560		(0)
a-disk-error	(0)
a-disk-recal	(0)

;;; Disk Characteristics (from label)
a-ncyls		(0)	;Total number of cylinders
a-nheads	(0)	;Number of data heads (i.e. tracks per cylinder)
a-nblks		(0)	;Number of blocks ("sectors") per track
a-heads-times-blocks	(0)	;Number of blocks per cylinder
a-disk-regs	(0)	;Virtual address of disk control registers
a-micr-origin	(0)	;Base disk address of microcode partition
a-micr-address	(0)	;Address of next block of microcode partition
a-micr-n-blocks	(0)	;Remaining blocks of microcode partition
a-tem1		(0)	;Temporary used in division

(locality i-mem)

(if prom (loc 0))
(if prom beg)
(if prom (jump go))

(loc 6)
i-mem-loc-6			;Define this symbol in either case.
(if prom   (jump-not-equal-xct-next q-r a-zero i-mem-loc-6)) ;This code also appears in the RAM
(if prom  ((q-r) add q-r a-ones))
(if prom i-mem-loc-10)
(if prom   (jump halt-cons i-mem-loc-10))	;Foo, should be in RAM by now
(if prom (error-table failed-to-jump-into-ram))

(if (not prom) (loc #o27000))
(if (not prom) beg)
(if (not prom) (jump go))

;;; Error halts:
;;; Put a NO-OP before each error halt so that they will be at even addresses
;;; and the 2 possible values in the PC lights will agree except for the low bit.

	(no-op)
error-bad-bit
	(jump halt-cons error-bad-bit)
    (error-table error-bad-bit)

	(no-op)
error-add-loses
	(jump halt-cons error-add-loses)
    (error-table error-add-loses)

	(no-op)
error-a-mem
	(jump halt-cons error-a-mem)
    (error-table error-a-mem)

	(no-op)
error-m-mem
	(jump halt-cons error-m-mem)
    (error-table error-m-mem)

	(no-op)
error-level-1-map
	(jump halt-cons error-level-1-map)
    (error-table error-level-1-map)

	(no-op)
error-page-fault
	(jump halt-cons error-page-fault)
    (error-table error-page-fault)

	(no-op)
error-bad-label
	(jump halt-cons error-bad-label)
    (error-table error-bad-label)

	(no-op)
error-no-micr
	(jump halt-cons error-no-micr)
    (error-table error-no-micr)

	(no-op)
error-bad-section-type
	(jump halt-cons error-bad-section-type)
    (error-table error-bad-section-type)

	(no-op)
error-bad-address
	(jump halt-cons error-bad-address)
    (error-table error-bad-address)

	(no-op)
error-end-of-partition
	(jump halt-cons error-end-of-partition)
    (error-table error-end-of-partition)

	(no-op)
error-disk-error
	(jump halt-cons error-disk-error)
    (error-table error-disk-error)

	(no-op)
error-divide-by-zero
	(jump halt-cons error-divide-by-zero)
    (error-table error-divide-by-zero)

	(no-op)
error-pdl-buffer
	(jump halt-cons error-pdl-buffer)
    (error-table error-pdl-buffer)

;;; Program starts here.  Create M-ZERO and M-ONES, and check the hardware a little.
go	((m-zero q-r) setz)		;Make all zeros.  Result to Q-R, not to depend on M mem

	(jump-if-bit-set (byte-field 1 #o0) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o1) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o2) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o3) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o4) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o5) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o6) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o7) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o10) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o11) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o12) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o13) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o14) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o15) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o16) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o17) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o20) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o21) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o22) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o23) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o24) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o25) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o26) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o27) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o30) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o31) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o32) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o33) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o34) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o35) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o36) q-r error-bad-bit)
	(jump-if-bit-set (byte-field 1 #o37) q-r error-bad-bit)

	((m-ones q-r) seto)		;Make all ones, in Q-R not to trust M Mem
	(jump-if-bit-clear (byte-field 1 #o0) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o1) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o2) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o3) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o4) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o5) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o6) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o7) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o10) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o11) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o12) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o13) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o14) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o15) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o16) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o17) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o20) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o21) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o22) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o23) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o24) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o25) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o26) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o27) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o30) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o31) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o32) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o33) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o34) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o35) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o36) q-r error-bad-bit)
	(jump-if-bit-clear (byte-field 1 #o37) q-r error-bad-bit)
;;; ALU and Shifter don't drop or pick bits.
;;; Test M-mem, A-mem, and M=A logic
	(jump-not-equal q-r a-ones error-a-mem)
	(jump-not-equal m-ones a-ones error-m-mem)
	((q-r) setz)
	(jump-not-equal q-r a-zero error-a-mem)
	(jump-not-equal m-zero a-zero error-m-mem)
;;; See if all carries in ALU really carry.
	((q-r) add m-ones a-zero alu-carry-in-one)
	(jump-not-equal q-r a-zero error-add-loses)
;;; Another simple carry test
	((q-r) add m-ones a-ones)
	(jump-if-bit-set (byte-field #o1 #o0) q-r error-add-loses)
	(jump-if-bit-clear (byte-field #o1 #o1) q-r error-add-loses)
	(jump-if-bit-clear (byte-field #o1 #o37) q-r error-add-loses)
;;; Prepare to test pdl buffer.  Care required since no pass-around path.
	((c-pdl-buffer-pointer-push) m-zero)
	((c-pdl-buffer-pointer-push) m-ones)
;;; This verifies that -1 + -1 is -2 and also tests the byte hardware a little
	((md) (byte-field #o37 1) q-r a-ones)
	(jump-not-equal md a-ones error-add-loses)
;;; Foo, the byte hardware could be tested a little bit better than that!
	(jump-not-equal c-pdl-buffer-pointer-pop a-ones error-pdl-buffer)
	(jump-not-equal c-pdl-buffer-pointer-pop a-zero error-pdl-buffer)

;;; Clear all memories to make sure they contain good parity
;;; But don't bash the M-ZERO and M-ONES constants.
	((m-hunoz) dpb m-ones (byte-field 1 5) a-zero)	;40, size of M memory
	((md) dpb m-ones (byte-field 1 2) a-zero)	;4, lowest location to clear
clear-m-memory
	((m-hunoz) add m-hunoz a-ones)	;Note that M-HUNOZ is used, to get good par in it
	((oa-reg-low) dpb m-hunoz (byte-field 5 14.) a-zero)	;M destination
	((m-garbage) m-zero)
	(jump-not-equal md a-hunoz clear-m-memory)

	((m-hunoz) dpb m-ones (byte-field 1 5) a-zero)	;40, size of M memory
clear-a-memory
	((oa-reg-low) dpb m-hunoz (byte-field 10. 14.) a-zero)	;A destination
	((a-garbage) m-zero)
	((m-hunoz) m+a+1 m-hunoz a-zero)
	(jump-if-bit-clear (byte-field 1 10.) m-hunoz clear-a-memory)

;;; Construct useful constants.
make-constants
	((a-1)   dpb m-ones (byte-field 1 0) a-zero)
	((a-2)   dpb m-ones (byte-field 1 1) a-zero)
	((a-3)   dpb m-ones (byte-field 2 0) a-zero)
	((a-4)   dpb m-ones (byte-field 1 2) a-zero)
	((a-5)   dpb m-ones (byte-field 1 2) a-1)
	((a-11)  dpb m-ones (byte-field 1 3) a-1)
	((a-40)  dpb m-ones (byte-field 1 5) a-zero)
	((a-400) dpb m-ones (byte-field 1 8) a-zero)
	((a-20000) dpb m-ones (byte-field 1 13) a-zero)
	((a-3560) dpb m-ones (byte-field 3 4) a-zero)
	((a-3560) dpb m-ones (byte-field 3 8) a-3560)
	;;; A-DISK-RECAL / 10001005
	((a-disk-recal) dpb m-ones (byte-field 1 9) a-5)
	((a-disk-recal) dpb m-ones (byte-field 1 21) a-disk-recal)
	;;; A-DISK-ERROR/ 47777360
	((a-disk-error) dpb m-ones (byte-field 04 04) a-zero)
	((a-disk-error) dpb m-ones (byte-field 12 09) a-disk-error)
	((a-disk-error) dpb m-ones (byte-field 01 23) a-disk-error)

;;; Clear some more memories
	((pdl-buffer-pointer m-a)	;2000, size of pdl buffer
		dpb m-ones (byte-field 1 10.) a-zero)
clear-pdl-buffer
	((c-pdl-buffer-pointer-push) m-zero)
	((m-a) add m-a a-ones)
	(jump-not-equal m-a a-zero clear-pdl-buffer)

	((m-a) a-40)			;Size of SPC stack
clear-spc-memory
	((micro-stack-data-push) setz)
	((m-a) add m-a a-ones)
	(jump-not-equal m-a a-zero clear-spc-memory)

	((md) setz)
clear-level-1-map
	((vma-write-map m-b) dpb m-ones (byte-field 1 26.) a-zero)	;Clear level-1 map
	((md) add md a-20000)
	(jump-if-bit-clear (byte-field 1 24.) md clear-level-1-map)

	((md) setz)
clear-level-2-map
	((m-a) (byte-field 5 13.) md)		;What to write in level-1 map
	((vma-write-map) dpb m-a (byte-field 5 27.) a-b) ;Make level-1 map point at level-2
	((vma-write-map) dpb m-ones (byte-field 1 25.) a-zero)	;Clear level-2 map
	((md) add md a-400)
	(jump-if-bit-clear (byte-field 1 24.) md clear-level-2-map)

	((m-a md) setz)
(if (not prom)
	((m-b) dpb m-ones (byte-field 3 9) a-zero) )	;27000 RAM version only
(if (not prom)
        ((m-b) dpb m-ones (byte-field 1 13.) a-b))
clear-i-memory
	((oa-reg-low) dpb md (byte-field 14. 12.) a-zero)
	(write-i-mem m-a a-a)
	((md) add md a-1)
(if prom
	(jump-if-bit-clear (byte-field 1 14.) md clear-i-memory) ;PROM, clear 16K
	(jump-less-than md a-b clear-i-memory) )                 ;RAM, clear to limit

	((m-a md) dpb m-ones (byte-field 1 17.) a-zero)	;0 with good parity
clear-d-memory
	((oa-reg-low) dpb md (byte-field 11. 12.) a-zero)
	(dispatch write-dispatch-ram (byte-field 0 0) (i-arg (a-mem-loc a-a)))
	((md) add md a-1)
	(jump-if-bit-clear (byte-field 1 11.) md clear-d-memory)

;;; The size of the disk is not known until the label is read in and parsed.
;;; In order to make the disk operations win, we have to initialize the
;;; size parameters to something reasonable; it doesn't matter what they
;;; are exactly since only blocks 0 and 1 will be dealt with.
fudge-initial-disk-parameters
	((a-nblks) a-3)
	((a-heads-times-blocks) a-3)

;;; Generate RESET on the I/O busses
	((interrupt-control) dpb m-ones		;RESET THE BUS INTERFACE AND I/O DEVS
		(byte-field 1 28.) a-zero)
	((m-a) a-400)				;RESET FOR ABOUT 80 MICROSECONDS
rst	(jump-not-equal-xct-next m-a a-zero rst)
       ((m-a) sub m-a a-1)
	((interrupt-control) dpb m-ones		;CLEAR RESET, SET HALFWORD-MODE,
		(byte-field 1 27.) a-zero)	;AND ENABLE INTERRUPTS

;;; Set up the map.  First, write a 0 into the first location of the level
;;; one map.  This is the only location we use.  Read back to be safe.
set-up-the-map
	((md) setz)
	((vma-write-map) dpb m-ones (byte-field 1 26.) a-zero)
	(no-op)			;Map gets written during this cycle
	((m-a) memory-map-data)
	((m-b) (byte-field 5 24.) m-a)
	(jump-not-equal m-b a-zero error-level-1-map)

;;; Set up four pages:
;;;  Page 0: Mapped to the first page of main memory.
;;;  Page 1: Mapped to the disk control registers.
;;;  Page 2: Mapped to the debug (spy) interface registers at 766000 .
;;;  Page 3: Mapped to the second page of main memory. (system communication area)
;;; M-A holds the bits to write the level 1 map with r/w access, MD has zero initially.
set-up-four-pages
	((m-a vma-write-map) dpb m-ones (byte-field 4 22.) a-zero);Sets bit 24 which is ignored

	((m-b) dpb m-ones (byte-field 11 0) a-a)
	((md) a-400)
	((vma-write-map) dpb m-ones (byte-field 4 12) a-b)	;M-A + 36777
	((a-disk-regs) dpb m-ones (byte-field 7 2) a-zero)	;Virt Addr 774

	((md) add md a-400)
	((m-b) dpb m-ones (byte-field 2 1) a-a)			;6
	((vma-write-map) dpb m-ones (byte-field 12 4) a-b)	;M-A + 37766

	((md) add md a-400)
	((vma-write-map) dpb m-ones (byte-field 1 0) a-a)	;M-A + 1
	(no-op)

	((vma) seto)			;Make sure page 0 has good parity
page-0-parity-fix			;so the disk doesn't barf out when we save it
	((vma-start-read) add vma a-1)
	(jump-if-page-fault error-page-fault)
			;Next instruction mustn't be ((VMA-START-WRITE) VMA).
	((write-memory-data-start-write) read-memory-data)
	(jump-if-page-fault error-page-fault)
	(jump-less-than vma a-400 page-0-parity-fix) ;This does one extra location, too bad.

;;; Now turn on parity checking
;;; Writing 4 in Unibus location 766012, which is at virtual address 1005,
;;; turns on ERROR-STOP-ENABLE.  If we aren't really in a PROM, we write 44
;;; which also turns on (leaves on) PROM-DISABLE.
(if prom
	((md) a-4)					;PROM
	((md) dpb m-ones (byte-field 1 5) a-4) )	;RAM
	((vma-start-write) dpb m-ones (byte-field 1 9) a-5)
	(jump-if-page-fault error-page-fault)

;;; Initialize the disk.
	(call disk-recalibrate)

;;; In order to not clobber core, save a page (page 0) of physical
;;; memory on disk.  Block 1 is allocated to us for this purpose.
save-a-page
	((m-a) dpb m-ones (byte-field 1 18.) a-zero)	;Delay for 0.1 second in case
sapdly	(jump-not-equal-xct-next m-a a-zero sapdly)	; something random is happening
       ((m-a) sub m-a a-1)
	((m-temp-1) a-1)
	(call-xct-next disk-write)
       ((m-a) setz)
	((m-temp-1) a-1)				;Now do a read-compare
	(call-xct-next disk-op-low)
       ((m-temp-2) dpb m-ones (byte-field 1 3) a-zero)	;OP 10 = read-compare
	((m-temp-1) and read-memory-data a-disk-error)
	(jump-not-equal m-temp-1 a-zero save-a-page)	;Got an error on readback, retry
	(jump-if-bit-set (byte-field 1 22.) read-memory-data save-a-page)	;R/C failed

;;; Read the label (block 0) into physical memory page 0, get disk
;;; parameters, and find out where the MICR partition is.
read-label
	((m-temp-1) setz)
	(call-xct-next disk-read)
       ((m-a) setz)

	;; M-B/ LABL = 114 102 101 114 = 11420440514
	((m-b) dpb m-ones (byte-field 2 02.) a-zero)
	((m-b) dpb m-ones (byte-field 1 06.) a-b)
	((m-b) dpb m-ones (byte-field 1 08.) a-b)
	((m-b) dpb m-ones (byte-field 1 14.) a-b)
	((m-b) dpb m-ones (byte-field 1 17.) a-b)
	((m-b) dpb m-ones (byte-field 1 22.) a-b)
	((m-b) dpb m-ones (byte-field 2 26.) a-b)
	((m-b) dpb m-ones (byte-field 1 30.) a-b)

decode-label
	((vma-start-read m-c) a-zero)		;0
	(jump-if-page-fault error-page-fault)	;First location of label must be ascii LABL
	(jump-not-equal read-memory-data a-b error-bad-label)
	((vma-start-read m-c) add m-c a-1)	;1
	(jump-if-page-fault error-page-fault)	;Second location of label must be version 1.
	(jump-not-equal read-memory-data a-1 error-bad-label)
	((vma-start-read m-c) add m-c a-1)	;2
	(jump-if-page-fault error-page-fault)
	((a-ncyls) read-memory-data)
	((vma-start-read m-c) add m-c a-1)	;3
	(jump-if-page-fault error-page-fault)
	((a-nheads) read-memory-data)
	((vma-start-read m-c) add m-c a-1)	;4
	(jump-if-page-fault error-page-fault)
	((a-nblks) read-memory-data)
	((vma-start-read m-c) add m-c a-1)	;5
	(jump-if-page-fault error-page-fault)
	((a-heads-times-blocks) read-memory-data)
	((vma-start-read m-c) add m-c a-1)	;6
	(jump-if-page-fault error-page-fault)
	((m-b) read-memory-data)
	((vma-start-read m-c)			;200
		dpb m-ones (byte-field 1 7) a-zero)
	(jump-if-page-fault error-page-fault)
	((m-d) read-memory-data)		;M-D gets number of partitions
	((vma-start-read m-c) add m-c a-1)	;201
	(jump-if-page-fault error-page-fault)
	((m-a) read-memory-data)		;M-A gets words per partition descriptor
	((m-c) add m-c a-1)			;202 (start of partition table)


	;; M-B/ Name of microload partition.
	;; M-C/ Address of partition descriptor.
	;; M-D/ Number of partitions
	;; M-A/ Number of words per partition descriptor.
search-label
	(jump-equal m-d a-zero error-no-micr)
	((vma-start-read) m-c)
	(jump-if-page-fault error-page-fault)
	(jump-equal read-memory-data a-b found-partition)
	((m-c) add m-c a-a)
	(jump-xct-next search-label)
       ((m-d) sub m-d a-1)

found-partition
	((vma-start-read m-c) add m-c a-1)
	(jump-if-page-fault error-page-fault)
	((a-micr-address) read-memory-data)
	((a-micr-origin) a-micr-address)
	((vma-start-read) add m-c a-1)
	(jump-if-page-fault error-page-fault)
	((a-micr-n-blocks) read-memory-data)
	((m-micr-offset) a-400)	;So will read in a new page first off

;;; Process one section.  Each section starts with three words:
;;; The section type, the initial address, and the number of locations.
;;; These are gotten into M-B, M-C, and M-D; then the section type is
;;; "dispatched" on.
;;; Section codes are:
;;; 1 = I-MEM, 2 = D-MEM, 3 = MAIN-MEM, 4 = A-M-MEM
process-section
	(call get-next-word)
	(call-xct-next get-next-word)
       ((m-b) m-a)
	(call-xct-next get-next-word)
       ((m-c) m-a)
	((m-d) m-a)
	(jump-equal m-b a-1 process-i-mem-section)
	(jump-equal m-b a-2 process-d-mem-section)
	(jump-equal m-b a-3 process-main-mem-section)
	(jump-equal m-b a-4 process-a-mem-section)
	(jump error-bad-section-type)

process-i-mem-section
	(jump-equal m-d a-zero process-section)
	((m-a) (byte-field 18. 14.) m-c)
	(jump-not-equal m-a a-zero error-bad-address)
	(call-xct-next get-next-word)
       ((m-d) sub m-d a-1)
	(call-xct-next get-next-word)
       ((m-b) m-a)
	;;; Now the first word of the instruction is in A-B, second word is in M-A,
	;;; and the address in I-MEM is in M-C.
	((oa-reg-low) dpb m-c (byte-field 14. 12.) a-zero)
	(write-i-mem m-a a-b)
	(jump-xct-next process-i-mem-section)
       ((m-c) add m-c a-1)

process-d-mem-section
	(jump-equal m-d a-zero process-section)
	((m-a) (byte-field 21. 11.) m-c)
	(jump-not-equal m-a a-zero error-bad-address)
	(call-xct-next get-next-word)
       ((m-d) sub m-d a-1)
	;;; Now M-A has the contents and M-C has the address.
	((oa-reg-low) dpb m-c (byte-field 11. 12.) a-zero)
	(dispatch write-dispatch-ram (byte-field 0 0) (i-arg (a-mem-loc a-a)))
	(jump-xct-next process-d-mem-section)
       ((m-c) add m-c a-1)

process-main-mem-section
	(call get-next-word)
	;;; M-C/ Number of blocks.
	;;; M-D/ Address of first block, relative to beginning of partition.
	;;; M-A/ Physical memory address of first word.
	((m-b) add m-d a-micr-origin)
main-mem-loop
	(jump-equal m-c a-zero process-section)
	(call-xct-next disk-read)
       ((m-temp-1) m-b)
	((m-b) add m-b a-1)
	((m-a) add m-a a-400)
	(jump-xct-next main-mem-loop)
       ((m-c) sub m-c a-1)

process-a-mem-section
	((pdl-buffer-pointer) sub m-c a-1)
a-mem-loop
	(jump-equal m-d a-zero done-loading)
	((m-a) (byte-field 22. 10.) m-c)
	(jump-not-equal m-a a-zero error-bad-address)
	(call-xct-next get-next-word)
       ((m-d) sub m-d a-1)
	(jump-xct-next a-mem-loop)
       ((c-pdl-buffer-pointer-push) m-a)

done-loading
;;; Read back page 0 of physical memory, which we saved on block 1.
	((m-temp-1) a-1)
	(call-xct-next disk-read)
       ((m-a) setz)
	((q-r) a-400)			;Set up constant needed below.
	((md) dpb m-ones (byte-field 1 5) a-4)	;44
	((vma) dpb m-ones (byte-field 1 9) a-5) ;1005

;;; Copy the PDL buffer into A/M memory.
	((pdl-buffer-index) setz)
fill-m-loop
	((oa-reg-low) dpb pdl-buffer-index (byte-field 5 14.) a-zero)
	((m-garbage) c-pdl-buffer-index)
	((pdl-buffer-index) m+1 pdl-buffer-index)
	(jump-if-bit-clear (byte-field 1 5) pdl-buffer-index fill-m-loop)
fill-a-loop
	((oa-reg-low) dpb pdl-buffer-index (byte-field 10. 14.) a-zero)
	((a-garbage) c-pdl-buffer-index)
	((pdl-buffer-index) m+1 pdl-buffer-index)
	(jump-not-equal pdl-buffer-index a-zero fill-a-loop)
;;; Turn off the PROM and enter the real world.
;;; Writing 44 in Unibus location 766012, which is at virtual address 1005,
;;; will turn on ERROR-STOP-ENABLE and PROM-DISABLE.  The 2 programs (PROM and RAM)
;;; share a common loop at location 6 to wait for this Unibus cycle to happen.
;;; Note that Q-R, MD, and VMA are already set up.

jump-to-6
	(jump-xct-next i-mem-loc-6)
       ((vma-start-write) vma)		;Well, we can't check for page fault here

get-next-word
;;; Get the next word of the MICR partition into M-A.  A-MICR-ADDRESS
;;; contains the number of the next page to be read in from it.
;;; A-MICR-N-BLOCKS has the number of remaining pages in it.
;;; M-MICR-OFFSET has the physical address of the next word.
;;; Clobbers M-TEMP-1, M-TEMP-2, Q-R.
	(jump-greater-or-equal m-micr-offset a-400 get-next-page)
	((vma-start-read) m-micr-offset)
	(jump-if-page-fault error-page-fault)
	(popj-after-next (m-a) read-memory-data)
       ((m-micr-offset) add m-micr-offset a-1)

get-next-page
	((m-micr-offset) setz)
	(jump-greater-or-equal m-zero a-micr-n-blocks error-end-of-partition)
	((a-micr-n-blocks) add m-ones a-micr-n-blocks)	;Subtract 1
	((m-temp-1) a-micr-address)
	(call-xct-next disk-read)
       ((m-a) setz)
	(jump-xct-next get-next-word)
       ((a-micr-address) m+a+1 m-zero a-micr-address)

;;; Disk commands.

;;; Initialize the disk drive.  First, wait for it to be on-line.
;;; Then do a fault clear and a recalibrate.  Then wait for the
;;; drive to become ready.
;;; Clobbers M-A, M-TEMP-1, M-TEMP-2.
disk-recalibrate
	((vma-start-read) a-disk-regs)		;Wait for control ready
	(jump-if-page-fault error-page-fault)
	(jump-if-bit-clear (byte-field 1 0) read-memory-data disk-recalibrate)
	((write-memory-data) a-zero)		;Select unit 0
	((vma) a-disk-regs)
	((vma-start-write) add vma a-2)		;Disk Address reg
	(jump-if-page-fault error-page-fault)
	((vma-start-read) a-disk-regs)
	(jump-if-page-fault error-page-fault)
	(jump-if-bit-set (byte-field 1 9.) read-memory-data disk-recalibrate)	;Off-line
    (error-table await-disk-on-line)		;Hangs near here until drive is on-line
	((m-a) setz)
	((m-temp-1) setz)
	((m-temp-2) a-5)
	(call-xct-next disk-op)
       ((m-temp-2) dpb m-ones (byte-field 1 8) a-temp-2)	;405 Fault Clear
	((m-temp-1) setz)
	(call-xct-next disk-op)
       ((m-temp-2) a-disk-recal)
await-drive-ready 
	((vma-start-read) a-disk-regs)
	(jump-if-page-fault error-page-fault)
	((m-temp-2) and read-memory-data a-3560)	;Bits 4,5,6,8,9,10
	(jump-not-equal m-temp-2 a-zero await-drive-ready)
    (error-table await-drive-ready)		;Hangs near here until drive is OK
	(popj)

;;; Read one block.
;;; Takes disk block number in M-TEMP-1, phys. mem. address in M-A
;;; Does not clobber M-A.
;;; Clobbers M-TEMP-1, M-TEMP-2, Q-R.
disk-read
	(jump-xct-next disk-op)
       ((m-temp-2) setz)

;;; Write one block.
;;; Takes disk block number in M-TEMP-1, phys. mem. address in M-A
;;; Does not clobber M-A.
;;; Clobbers M-TEMP-1, M-TEMP-2, Q-R.
disk-write
	((m-temp-2) a-11)
	; drops in.

disk-op
	(call disk-op-low)
	((m-temp-1) and read-memory-data a-disk-error)
	(jump-not-equal m-temp-1 a-zero error-disk-error)
	(popj)

disk-op-low
	;; Wait for the disk controller to be ready.
	((vma-start-read) a-disk-regs)
	(jump-if-page-fault error-page-fault)
	(jump-if-bit-clear (byte-field 1 0) read-memory-data disk-op)
    (error-table await-disk-control-ready)	;Hangs near here if control hung or absent
	
	;; Write a CCW word to the last loc of sys. comm. area.
	((md) selective-deposit m-a (byte-field 16. 8.) a-zero)
	((vma-start-write) dpb m-ones (byte-field 10. 0) a-zero)	;Virt Addr 1777
	(jump-if-page-fault error-page-fault)

	;; Write the COMMAND register from M-TEMP-2.
	((md) m-temp-2)
	((vma-start-write) a-disk-regs)
	(jump-if-page-fault error-page-fault)
	
	;; Write the CLP register.
	((md) dpb m-ones (byte-field 9 0) a-zero)	;Phys Addr 777 = Virt Addr 1777
	((vma-start-write) add vma a-1)
	(jump-if-page-fault error-page-fault)

disk-op-continued
	;; Convert block number in M-TEMP-1 to DISK ADDRESS word.
	;; Write the DISK ADDRESS register.
	(call-xct-next div)
       ((m-temp-2) a-heads-times-blocks)
        ((md) dpb q-r (byte-field 12. 16.) a-zero)
	(call-xct-next div)
       ((m-temp-2) a-nblks)
	((m-temp-1) dpb q-r (byte-field 8 8) a-temp-1)
	((md) ior md a-temp-1)
	((vma-start-write) add vma a-1)
	(jump-if-page-fault error-page-fault)

	;; Write the START register.
	((vma-start-write) add vma a-1)
	(jump-if-page-fault error-page-fault)

	;; Wait for operation to complete.
disk-wait	
	((vma-start-read) a-disk-regs)
	(jump-if-page-fault error-page-fault)
	(jump-if-bit-clear (byte-field 1 0) read-memory-data disk-wait)
    (error-table await-disk-done)		;Hangs near here while waiting for disk
	(popj)

;;; Divide two numbers.  This routine taken from UCADR 108.
;;; Dividend in M-TEMP-1, divisor in M-TEMP-2
;;; Quotient In Q-R, remainder in M-TEMP-1
;;; Clobbers A-TEM1.

div	(jump-greater-or-equal-xct-next m-temp-1 a-zero div1)
       ((a-tem1 q-r) m-temp-1)
	((q-r) sub m-zero a-tem1)
div1	((m-temp-1) divide-first-step m-zero a-temp-2)
div1a	(jump-if-bit-set (byte-field 1 0) q-r error-divide-by-zero)
(repeat 31. ((m-temp-1) divide-step m-temp-1 a-temp-2))
	((m-temp-1) divide-last-step m-temp-1 a-temp-2)
	(jump-less-or-equal-xct-next m-zero a-tem1 div2)
       ((m-temp-1) divide-remainder-correction-step m-temp-1 a-temp-2)
	((m-temp-1) sub m-zero a-temp-1)
div2	((a-tem1) xor m-temp-2 a-tem1)
	(popj-less-or-equal m-zero a-tem1)
	(popj-after-next
	 (a-tem1) q-r)
       ((q-r) sub m-zero a-tem1)
)))
