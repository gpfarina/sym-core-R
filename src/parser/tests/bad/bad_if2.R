# test whether the else context sensitivity hack broke error reporting
{
    if (TRUE) 1


    else (1
    
    1) # malformed expression
}
