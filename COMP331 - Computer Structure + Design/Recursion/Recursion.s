#Authors: Hanna Elszasz and Fabien Bessez
#MIPS Recursion Assign 5 COMP331

.data 

m: .word 0, 1, 2, 3
n: .word 0, 1, 2, 3
OutputMsg1: .asciiz "f1("
OutputMsg2: .asciiz ","
OutputMsg3: .asciiz ") ="
OutputMsg4: .asciiz "f2("
newLine: .asciiz "\n"
.text

main: li $t0, 1 #for comparison and adding one in computation
	  la $t1, m 
	  la $t2, n
	  li $t3, 0 #counter for n
	  li $t4, 3 #for comparison
	  li $t5, 0 #counter for m
	  li $t6, 2 #used for calculation f2
	  li $s0, 0
	  li $t7, 4 

loop:li $v0, 4 # print that message
	 la $a0, OutputMsg1 # load outputMsg1 into $a0
	 syscall

	 li $v0, 1 # print this number
	 lw $a0, ($t1) #load the value of the first argument into $a0 
	 syscall

	 li $v0, 4 #print this message
	 la $a0, OutputMsg2 #load outputmsg2 into $a0
	 syscall

	 li $v0, 1 #print this number
	 lw $a0, ($t2) #put the value of the second argument into $a0
	 syscall

	 li $v0, 4 #print this message
	 la $a0, OutputMsg3 #load outputmsg3 into $a0
	 syscall 

	 lw $a0, ($t1) #load value of m as first argument 
	 lw $a1, ($t2) #load value of n as second argument
	 jal f1_start #calling f1 with these arguments
	 add $a0, $v0, $zero #store the result in $a0
	 li $v0, 1 #print this number
	 syscall

	 li $v0, 4 #print this message
	 la $a0, newLine #load newLine into $a0
     syscall

	 beq $t4, $t3, increment_m_and_n
	 beq $t7, $t5, finish

	 j increment_n

increment_n: addi $t3, $t3, 1 #add one to the n counter
			 addi $t2, $t2, 4 #increment n
			 j loop

increment_m_and_n: li $t3, 0 #reset n's counter
				   addi $t1, $t1, 4 #go to the next value of m
				   addi $t5, $t5, 1 #increment m's counter by 1
				   addi $t2, $t2, -12 #start at begining of n
				   j loop

finish: 
		lw $a1, 0($sp)
		lw $a0, 4($sp)
		lw $ra, 8($sp)
		addiu $sp, $sp 12
		jr $ra

		# li $v0, 10 #terminate the program
	    # syscall


f1_start: addiu $sp, $sp, -12 #make room on the stack
          sw $ra, 8($sp) #store the return address on the stack
          sw $a0, 4($sp) #store the first argument on the stack
          sw $a1, 0($sp) #store second argument on the stack

          bne $a0, $0, elseif #  if m==0 then go to elsif
          addiu $a1, $a1, 0
          j finish # answer = n

elseif: bne $a1, $t0, else # the n = 1
		sll $v0, $t0, $a0
		jal f1_start
		j finish

else2: addiu $a1, $a1, 1 #arg 1 = n+1
	   addiu $s0, $s0, -1 
	   jal f1_start
	   move $s0 $s0
	   move $a1, $v0
	   jal f1_start

.end main
