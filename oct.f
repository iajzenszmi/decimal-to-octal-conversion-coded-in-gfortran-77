    program OctalConversion
    implicit none
    integer :: i

    do i = 1, 10
        print *, 'Decimal:', i, 'Octal:', decimal_to_octal(i)
    end do

contains

    function decimal_to_octal(n) result(octal)
        implicit none
        integer, intent(in) :: n
        character(len=20) :: octal
        integer :: temp, i
        character(len=1) :: oct_digit

        temp = n
        octal = ""
        do while (temp /= 0)
            i = mod(temp, 8)
            write(oct_digit, '(I1)') i
            octal = trim(adjustl(oct_digit)) // octal
            temp = temp / 8
        end do
        if (len_trim(octal) == 0) octal = "0"
    end function decimal_to_octal

end program OctalConversion

