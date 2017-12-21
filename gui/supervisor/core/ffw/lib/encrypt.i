/*-----------------------------------------------------------------------*
File........: encrypt.i
Version.....: 1.1 - 11/16/2000
Description : Adds the encrypt() and decrypt() functionality to 4gl programs
Input Param : <none>
Output Param: <none>
Author......: S.E. Southwell -  United Systems, Inc. (214) 488-2239
Copyright...: FreeFramework 2000 - http://www.freeframework.org
Created.....: 10/5/2000
Notes.......: If you make changes to this program that others could benefit
              from, please share your code with me: ses@usiatl.com
              
              PLEASE NOTE:  Strings encrypted with versions prior to 1.1 cannot
              be decrypted with this algorithm.  Please decrypt any stored data
              with the old version, then re-encrypt with this one.
Modified....: 
        11/16/2000 - Added extra hashing so that the encryption shingle cannot be inferred
              by examining the encryption of a known string.  Also added deallocation of
              the memory for myraw in encrypt() and decrypt().
        11/13/2000 - Thanks to Lubomir Mrazik for finding a bug that was being caused
              by poor handling of null (ascii=0) characters within intermediate results.
              SES fixed by using a memptr for the working copy of the encryption-in-process.
              CRC check also added back in.
        11/02/2000 - Thanks to Mario Paranhos for finding a bug that 
              was causing mess-ups of the upper characters and also chr(255).
              Turns out that zeros weren't being padded to the translated single-digit
              lower ascii characters after the conversion to base 36.              
*-----------------------------------------------------------------------*/

/*IMPORTANT: For each unique implementation of this library, change the redherring. */
/* - This prevents brute-force attacks on encrypted data from being successful 
     without first obtaining your .r code    */
&GLOBAL-DEFINE redherring "foobar":U
/*Shingle size defines the theoretical number of guesses it would take to assure cracking
  the encryption key: (Benchmarks on AMD K6/2 366 Winnt 4.0, WS 2.1)
  Size - Step          Guesses   Run Time in ms/KB   
  -----------  ---------------- ------------------ 
    5     1                   52     Not Secure        
    8     2                2,704       3055       
    9     2                2,704       3055       
   12     3              140,608       2995  
   15     4            7,311,616       2934  
   16     5          380,204,032       2834   
   16     6       19,770,609,664       2754 
          7    1,028,071,702,528
          8   53,459,728,531,456  
          16   A whole lot        ~

  Step adds a bit of randomness to the encoded text by increasing the spread
  of the ASCII characters to occupy the full 0 - 255 range so that a statistical
  analysis of encoded text would show less significant results.  The fullness of 
  this range is determined by the ratio of size to step.  High ratios are more
  fullness and more secure.
  About (1/3 * shinglesize) (round downward) is a good figure to go with.*/
  
&GLOBAL-DEFINE shinglesize 16
&GLOBAL-DEFINE step 5

{ ffw/lib/base36.i }


FUNCTION encrypt returns char(
 INPUT mystring AS CHAR,
 INPUT mykey AS CHAR
):
    DEFINE VAR i            AS INTEGER NO-UNDO.
    DEFINE VAR j            AS INTEGER NO-UNDO.
    DEFINE VAR keylength    AS INTEGER NO-UNDO.
    DEFINE VAR stringlength AS INTEGER NO-UNDO.
    DEFINE VAR mycrc        AS CHAR    NO-UNDO.
    DEFINE VAR myraw        AS MEMPTR  NO-UNDO.
    DEFINE VAR myhash       AS CHAR    NO-UNDO.
    
    SET-SIZE(myraw) = (length(mystring) + 1).
    ASSIGN
     mykey = substring(encode(mykey + {&REDHERRING}),1,{&shinglesize})
     keylength = LENGTH(mykey,"raw")
     stringlength = LENGTH(mystring,"raw")
     mycrc = SUBSTRING(ENCODE(mystring + {&REDHERRING}),1,2,"raw")
     myhash = ENCODE(mycrc + {&REDHERRING} + mykey)
     put-string(myraw,1) = mystring
    .

    /*Now Encrypt it*/
    DO i = 0 TO stringlength - 1 BY {&step}:
        DO j = 1 TO MIN(keylength,stringlength - i):
        put-byte(myraw,(i + j)) = (get-byte(myraw,(i + j)) + ASC(substring(mykey,j,1,"raw"))) modulo 256.
        END.
    END.
    
    /*Now Hash it up so that it's not reversible by 
    comparing an encrypted version of a known string*/
    
    DO i = 1 TO stringlength:
        put-byte(myraw,i) = (get-byte(myraw,i) + ASC(substring(myhash,(i modulo 16) + 1,1,"raw"))) modulo 256.
    END.
    
    
    ASSIGN mystring = "".
    DO i = 1 TO stringlength:
        ASSIGN mystring = mystring + zeropad(tobase36(get-byte(myraw,i)),2).
    END.
    
    /*Deallocate the memory*/
    SET-SIZE(myraw) = 0.
    
    /*Return the encoded string with CRC info*/
    RETURN  mycrc + mystring.
end FUNCTION.

FUNCTION decrypt returns char(
 INPUT mystring AS CHAR,
 INPUT mykey AS CHAR
):
    DEFINE VAR i            AS INTEGER NO-UNDO.
    DEFINE VAR j            AS INTEGER NO-UNDO.
    DEFINE VAR keylength    AS INTEGER NO-UNDO.
    DEFINE VAR stringlength AS INTEGER NO-UNDO.
    DEFINE VAR mycrc        AS CHAR    NO-UNDO.
    DEFINE VAR myhash       AS CHAR    NO-UNDO.
    DEFINE VAR debugvar     AS CHAR    NO-UNDO.
    DEFINE VAR myraw        AS MEMPTR  NO-UNDO.
    
    ASSIGN
     mycrc = SUBSTRING(mystring,1,2)
     mystring = substring(mystring,3,-1,"raw")
    .
    SET-SIZE(myraw) = (length(mystring)).

    ASSIGN
     mykey = substring(encode(mykey + {&REDHERRING}),1,{&shinglesize})
     myhash = ENCODE(mycrc + {&REDHERRING} + mykey)
     keylength = LENGTH(mykey,"raw")
     stringlength = LENGTH(mystring,"raw")
    .
     
    /*read it into raw...*/
    DO i = 1 to stringlength BY 2:
        ASSIGN j = j + 1. /*byte counter*/
        put-byte(myraw,j) = to10from36(substring(mystring,i,2,"raw")).
    END. 
    ASSIGN stringlength = INTEGER(stringlength / 2).
    
    /*First de-hash it*/
    DO i = 1 TO stringlength:
        put-byte(myraw,i) = abs((get-byte(myraw,i) - ASC(substring(myhash,(i modulo 16) + 1,1,"raw"))) modulo 256).
    END.

    
    /*Now Decrypt it*/
    DO i = 0 TO stringlength - 1 BY {&step}:
        DO j = 1 TO MIN(keylength,stringlength - i):
        put-byte(myraw,(i + j)) = abs((get-byte(myraw,(i + j)) - ASC(substring(mykey,j,1,"raw"))) modulo 256).
        END.
    END.
    ASSIGN mystring = "".
    DO i = 1 TO stringlength:
        ASSIGN mystring = mystring + chr(get-byte(myraw,i)).
    END.
    
    /*Deallocate the memory*/
    SET-SIZE(myraw) = 0.
    
    /*Return results*/
    IF mycrc = SUBSTRING(ENCODE(mystring + {&REDHERRING}),1,2,"raw") THEN RETURN  mystring.
    ELSE RETURN "".
end FUNCTION.



