ones n = take n (cycle [1])

assignToGroups n aList = zip groups aList
    where groups = cycle [1..n]

fileGroups = assignToGroups 3 ["file1.txt","file2.txt","file3.txt"
                     ,"file4.txt","file5.txt","file6.txt","file7.txt"
                     ,"file8.txt"]
-- Q6.1
myRepeat item = cycle [item]

-- Q6.2
subseq start end aList =
    take (end - start) (drop start aList)

-- Q6.3
inFirstHalf item aList =
    item `elem` firstHalf
    where
        firstHalf = take (length aList `div` 2) aList