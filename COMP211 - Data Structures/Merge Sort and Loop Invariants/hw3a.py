
def merge(xs, k, size):

    m = k + size
    n = k + 2*size
    i = k
    j = m

    res = []    # type: List[int]
    while i < m and j < n:
        if xs[i] <= xs[j]:                # key comparison: 1
            list.append(res, xs[i])       # how many times through loop?
            i = i + 1                     # I think it goes through
        else:                             # 1/2 len(xs) times
            list.append(res, xs[j])
            j = j + 1

    if i == m:                            # key comparison: 2
        list.extend(res, xs[j:n])         # happens just once each call
    else:
        list.extend(res, xs[i:m])

    xs[k:n] = res
    
    return None

def merge_sort(xs):
    
    if len(xs) <= 1:                      # key comparison: 1
        return None

    size = 1
    while size < len(xs):
        i = 0
        while i < len(xs): 
            merge(xs, i, size)     # 1 + ([1/2 len(xs)] + 1) * 1/2 size
            i = i + 2*size         # or 1 + [len(xs) + 1] * [1/2 size]
        size = 2*size


    return None


# worst case scenario is the array is size n and n[0] > n[1] > n[2]...n[n-1]
# merge sort has set-up cost of 1 key comparison
# then it just calls merge
# 
# cost of loop = (cost of one iteration) x (number of iterations)
#cost of merge sort is 1 + cost of inner loop = merge
# merge is 1 + cost of merge_loop
# merge_ loop is 1/2 len(xs) = 1/2 size
so 1 + (len(xs) /2) * 1/2 size

def radix_sort(xs):

    # print("radix_sort(%r)" % xs)
    if xs == []:                            #key comparison: 1
        return None

    # k is the number of digits in each numeral.
    k = len(xs[0])
    # print("\tk = %d" % k)

    # i is the digit upon which we are sorting.
    i = 0

    while i < k:
        # print("\ti = %d" % i)
        # pfx is the prefix of the subsequence we will sort into bins.
        pfx = xs[0][:i]

        # xs[j] is the first element of xs with prefix pfx.
        j = 0
        while j < len(xs):
            bin0 = [] # type: List[List[int]]           
            bin1 = [] # type: List[List[int]]
            jj = j

            # Scan through elements of xs with prefix pfx, sorting into bin0
            # and bin1.  When done, xs[j:jj] will have been sorted into the
            # bins.  Note that we are guaranteed that upon finishing this
            # loop, jj > j.
            while jj < len(xs) and xs[jj][:i] == pfx:
                if xs[jj][i] == 0:                      #key comparison: 1
                    bin0.append(xs[jj])                 # cost : 1
                else:                                   #key comparison: 2??
                    bin1.append(xs[jj])                 # cost: 1
                jj += 1

            # Replace xs[j:jj] with bin0+bin1.
            xs[j:jj] = bin0 + bin1                      # assignment 



            # If we haven't made it through xs yet, update the prefix.
            if jj < len(xs):                            # key comparison: 3
                pfx = xs[jj][:i]

            # Since jj > j, this is guaranteed to increase j.
            j = jj

        i = i + 1

    return None













