/*----------------------------------------------------------------------
    File        : create.i    
    Purpose     : Assign audit fields on record create.
    Syntax      :
    Description :
    Author(s)   : S.E. Southwell
    Created     : 16 December 1999
    Notes       :
    Last Modified:
----------------------------------------------------------------------*/
{&tablename}.add-dt = TODAY
{&tablename}.add-time = TIME
{&tablename}.add-userid = (if avail webSession and websession.webuser ne "" then webSession.webUser else "WebSpeed")

/*end*/

