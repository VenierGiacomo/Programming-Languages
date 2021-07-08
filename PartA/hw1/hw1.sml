(* Homework1 *)

(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false) *)
fun is_older((a:int, b:int, c:int), (x:int, y:int, z:int)) =
    if (x > a)
    then true
    else if (x = a andalso y > b)
        then true
        else if (y = b andalso z > c)
            then true
            else false

(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month. *)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null date
    then 0
    else 
        let  
            val remainig_date = number_in_month(tl dates, month)
        in 
            if #2 (hd dates) = month
            then 1 + number_in_month(remainig_date, month)
            else number_in_month(remainig_date, month)
        end

(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)

fun number_in_months(date: (int*int*int) list, months: int list) = 
    if null months
    then 0
    else number_in_month(date, hd months) + number_in_months(date, tl months )

 (* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given. *)
fun dates_in_month(date: (int*int*int) list, month: int) =
        if null date
        then []
        else
            let
                val remainig_dates = dates_in_month(tl dates, month)
            in
            if #2 (hd dates) = month
            then (hd dates) :: remainig_dates
            else remainig_dates
        end
(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem and SML’s list-append operator (@) *)
fun dates_in_months(date: (int*int*int) list, months: int list) = 
    if null months
    then []
    else dates_in_month(date, hd months ) @ dates_in_months(date, tl months )

(* 6. . Write a function get_nth that takes a list of strings and a positive int n and returns the nth element of the list where the head of the list is 1st . Do not worry about the case where the list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth(strings: string list, n: int) =
     if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)
    
(* 7. Write a function date_to_string that takes a date and returns a string of the form September-10-2015 (for example). Use the operator ^ for concatenating strings and the library function Int.toString for converting an int to a string. For producing the month part, do not use a bunch of conditionals. Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, use hyphens exactly as in the example and use English month names: January, February, March, April, May, June, July, August, September, October, November, December. *)
fun date_to_string (yr: int, month:int, day: int) =
    let
        val months = ["January", "February", "March", "April","May", "June", "July",
                     "August", "September", "October", "November", "December"]
    in
        get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(yr)
    end

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume is positive, and an int list, which you can assume contains all positive numbers, and returns an int. You should return an int n such that the first n elements of the list add to less than sum, but the first n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in value; it is okay for an exception to occur if this is not the case. *)
fun number_before_reaching_sum(sum: int, sums: int list) =
    if hd numbers >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

 (* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your answer to the previous problem. *)
fun what_month(dayOfAYear: int) = 
    let
        val monthsOfAYear   =   [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val monthsOfALeapYear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun  what_month_inner(monthsOfAYear_: int list, remainingDays: int, month: int) = 
            if dayOfAYear = 0
            then 0
            else
                if dayOfAYear = hd(monthsOfAYear_) orelse remainingDays < hd(monthsOfAYear_)
                then month
                    else what_month_inner(tl(monthsOfAYear_), remainingDays-hd(monthsOfAYear_), month+1)
    in
        what_month_inner(monthsOfALeapYear, dayOfAYear, 1)
    end

(* 10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1day2. *)
fun month_range(day1: int, day2: int) = 
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)

(* 11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It evaluates to NONE if the list has no dates else SOME d where the date d is the oldest date in the list. *)
fun oldest(dates: (int*int*int) list) = 
    if null dates
    then NONE
    else
        let
          fun oldest_nonempty(dates_ : (int * int * int) list) =
            if null (tl dates_)
            then hd dates_
            else
                let
                    val tl_oldest = oldest_nonempty(tl dates_)
                in
                    if is_older(hd dates_, tl_oldest)
                    then hd dates_
                    else tl_oldest
                end
      in
          SOME (oldest_nonempty(dates_))
      end

(* 12. Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge that are like your solutions to problems 3 and 5 except having a month in the second argument multiple times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.) *)

fun general_remove_duplic(months : int list) =
  let
      val sorted_months = sort months
  in
      if null sorted_months
      then []
      else if null (tl sorted_months)
      then sorted_months
      else
          let
              val tl_remove_duplicates = general_remove_duplic(tl sorted_months)
          in
              if hd sorted_months = hd tl_remove_duplicates
              then tl_remove_duplicates
              else (hd sorted_months) :: tl_remove_duplicates
          end
  end

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
  number_in_months(dates, general_remove_duplic(months))

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
  dates_in_months(dates, general_remove_duplic(months))

(* 13.  Challenge Problem: Write a function reasonable_date that takes a date and determines if it describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100. (Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)
fun reasonable_date(date : int * int * int) =
  let
      fun is_leapyear(year : int) =
        year mod 4 = 0 andalso year mod 100 <> 0 orelse year mod 400 = 0
      val leap_month_limit = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      val normal_month_limit = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      fun get_nth(numbers : int list, n : int) =
        if n = 1
        then hd numbers
        else get_nth(tl numbers, n - 1)
  in
      if #1 date < 1 orelse #3 date < 1 orelse #3 date > 31orelse #2 date < 1 #2 date > 12 orelse
      then false
      else if is_leapyear(#2 date)
      then #3 date <= get_nth(leap_month_limit, #2 date)
      else #3 date <= get_nth(normal_month_limit, #2 date)