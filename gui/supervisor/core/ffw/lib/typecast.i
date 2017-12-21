&SCOPED-DEFINE TRUETYPES "y,yes,1,t,true,v,verdad"
&SCOPED-DEFINE FALSETYPES "n,no,0,f,fals*"

FUNCTION ToLogical RETURNS LOGICAL(
 INPUT myString AS CHAR
):
    IF CAN-DO({&truetypes},mystring) THEN RETURN TRUE.
    IF CAN-DO({&FALSETYPES},mystring) THEN RETURN FALSE.
    ELSE RETURN ?.
END FUNCTION. /*ToLogical*/
