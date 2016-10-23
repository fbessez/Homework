.data

string: .asciiz "The probability of randomly selecting an unlucky time is "
quotient: .asciiz "/"

.text

main:
	li $t0, 10 # divider number t2
	li $t1, 13 # max hours t0
	li $t2, 60 # max minutes t1
	li $s0, 13 # unlucky number is this number s0
	li $s1, 721 # total minutes in 12 hours s2
	li $s2, 1 # hours s3
	li $s3, 0 # register to count total unlucky numbers gone through s7
	li $s4, 0 # total minutes gone through s4

start:
	div $s2, $t0 # divide total hours by 10
	mfhi $t3 # mflo gets the first digit in hours from the div and stores into t3
	mflo $t4 # mfhi gets the secnod digit in hours from the div and stores into t4
	div $s4, $t0 # divide total minutes by 10
	mfhi $t5 #mflo gets the first digit in minutes from the div and stores into t5
	mflo $t6 #mfhi gets the second digit in minutes from the div and stores into t6
	add $s5, $t3, $t4 # add the first two hour digits and store into s1
	add $s5, $s5, $t5 # add the first minute digit to the sum of the hour digits and store into s1
	add $s5, $s5, $t6 # add the second minute digit to the sum of the previosu three digits and store into s1
	beq $s5, $s0, it_is_unlucky # if the sum of all four digits is 13 then send program to the counter
	j minute_tick # else move to minute_tick loop

minute_tick: #this should make the minutes increase by 1
	addi $s4, $s4, 1 # add 1 to minutes each cycle
	beq $s4, $t2, minute_restore # if 60 minutes have passed, go to minute_restore
	j start

minute_restore: # this should reset minutes to 0
	sub $s4, $s4, $t2 # subtract 60 from it --> perhaps just reset it to 0 by li $s4, 0 ??
	j hour_tick # go to hour tick now

hour_tick: # this hsould make the hours increase by 1 
	addi $s2, $s2, 1 # increase by 1 the hours 
	beq $s2, $t1, finally # if total hours is 13 then go to finally (because we are done with 12 hours) ####### change t1 for s0
	j start  # else

it_is_unlucky: # counts total unlucky tiems
	addi $s3, $s3, 1 # add 1 to counter and store it in counter
	j minute_tick

finally:
	la $a0, string # load the string
	li $v0, 4 # print that message
	syscall
	addi $a0, $s3, 0 # load how many total unlucky numbers there are into a0
	li $v0, 1 # print that integer
	syscall
	la $a0, quotient # load the slash symbol
	li $v0, 4 # print the string containing the slash
	syscall
	addi $a0, $s1, -1 # store the number 720 (total minutes possible) in a0
	li $v0, 1 # print that integer
	syscall
	li $v0, 10 # end program
	syscall

.end main



	
	



















