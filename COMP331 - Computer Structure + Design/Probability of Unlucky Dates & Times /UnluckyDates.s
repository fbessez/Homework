.data
months_and_hours: .word 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9   
					   # the sum of the digits for all months and hours (
					   # 2 1's for 10 and 01 etc.)
dates: .word 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 3, 4
OutputMsg: .asciiz "the probability of randomly selecting an unlucky date is "
quotient: .asciiz "/"
.text

main: 
	li $t0, 31 # tells loop when to stop looking for unlucky dates in that month
	li $s0, 13 # comparison to tell if a date or time is unlucky
	li $s1, 366 # total days in a year
	li $s3, 11 # when to switch first digit
	la $t6, dates
	la $t7, months_and_hours
	li $t9, 0 #counter_dates
	li $s4, 0 #unlucky_dates
	li $t8, 0 #days in that month gone thruogh
	li $s7, 1

loop_month:
	beq $t8, $t0, next_value #if the counter has reached 32, move to the next month
	j loop_date #if not, move on to loop_date which adds the digits

next_value:
	li $t8, 0 #reset it to 0
	addi $t7, $t7, 4 #increase the address of the months_and_hours array by 4 to move to the next month
	addi $t6, $t6, -124 #go back to the begining of dates
	j loop_date #move on to loop_date to add new digits

loop_date:
	lw $t3, ($t6) # load digit into t3
	lw $t2, ($t7) # load month into t2
	j increment_counter_dates 

increment_counter_dates:
	addi $t8, $t8, 1 # add one to the counter
	addi $t9, $t9, 1 #add one to the counter
	beq $t9, $s1, subtract1 #if the counter has reached 366, move to divide
	j unlucky_or_lucky_dates #if not, check and see if the date is unlucky 

unlucky_or_lucky_dates:
	add $t5, $t2, $t3 # adds month and day digits
	beq $t5, $s0, unlucky_dates_loop #if t5 is 13 then add one instance of unluckiness to our accumulator
	j next_day #if not, start the whole process over

unlucky_dates_loop:
	addi $s4, $s4, 1 #add one to the counter of unlucky days
	j next_day #start at the whole process over

next_day: 
	addi $t6, $t6, 4 #everytime through the loop, the day is advanced by one
	j loop_month #go back to the begining

subtract1:
	sub $s1, $s1, 1 #subtract 1 from 366 to get 365
	sub $s4, $s4, 1 #subtract 1 from total unlucky dates because of 02/29
	j finish1 #go to finish

finish1:
	li $v0, 4 # print that message
	la $a0, OutputMsg # load outputMsg into $a0
	syscall

	li $v0, 1 # print this number
	add $a0, $s4, $zero #store in $a0
	syscall

	li $v0, 4 #print this message (/)
	la $a0, quotient #store in a0 
	syscall

	li $v0, 1 #print 365
	add $a0, $s1, $zero # store in z0
	syscall

	li $v0, 10 #terminate the program
	syscall
	
.end main


