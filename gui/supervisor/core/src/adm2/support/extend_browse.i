/****************/
/* SALIDA A PDF */
/****************/
{pdfprint.i}
function CalcColumn returns character (ip_hQuery as handle, ip_iRow as int, ip_iCol as int) forward.

/***************************/
/* MENU POPUP EN EL BROWSE */
/***************************/
define menu pp-menu-browse title "Men£ Browse"
       menu-item opc_pdf   label "&Generar PDF"
       menu-item opc_csv   label "Generar Delimitado CS&V".
assign {&BROWSE-NAME}:popup-menu in frame {&FRAME-NAME} = menu pp-menu-browse:handle.

/************************/
/* EVENTO: GENERAR PDF  */
/************************/
on "CHOOSE" of menu-item opc_pdf
 do:
  /* find first userdb._user where userdb._user._userid = userid("userdb") NO-LOCK NO-ERROR. */
   find first TTPDF.
   assign TTPDF.creporttitle   = trim(frame {&FRAME-NAME}:title)
          TTPDF.hbrowse        = {&BROWSE-NAME}:HANDLE IN FRAME {&FRAME-NAME}
          TTPDF.cUserCode      = /* if userdb._user._user-name = "" or userdb._user._user-name = ? then 
                                   if userid("userdb") <> "computos" then
                                     substring(userid("userdb"),3)
                                   else
                                     userid("userdb")
                                 else
                                    userdb._user._user-name */ "usuario"
          TTPDF.cPageLayout    = if {&BROWSE-NAME}:num-columns > 5 then "Landscape" else "Portrait"
          TTPDF.cPaperSize     = if {&BROWSE-NAME}:num-columns > 10 then "A3" else "A4"
          TTPDF.loAltColours   = yes
          TTPDF.deAltColour[1] = DEC(229)
          TTPDF.deAltColour[2] = DEC(229)
          TTPDF.deAltColour[3] = DEC(229)
          TTPDF.cFunction[1]   = "CalcColumn".
   if TTPDF.creporttitle = "" or TTPDF.creporttitle = ? then
     TTPDF.creporttitle = "Reporte con Contenido del Browse".
   /* release userdb._user. */
   run pdfseleccion.p (input-output TTPDF.cPageLayout, input-output TTPDF.cPaperSize, input-output TTPDF.loAltColours).
   {s_pausa1.i}
   run pdflibrary.p (INPUT-OUTPUT TABLE TTPDF).
   {s_pausa2.i}
 end.
    
/************************/
/* EVENTO: GENERAR CSV  */
/************************/
on "CHOOSE" of menu-item opc_csv
 do:
 /*  find first userdb._user where userdb._user._userid = userid("userdb") no-lock.    */
   find first TTPDF.
   assign TTPDF.creporttitle   = trim(frame {&FRAME-NAME}:title)
          TTPDF.hbrowse        = {&BROWSE-NAME}:HANDLE IN FRAME {&FRAME-NAME}
          TTPDF.cUserCode      = /*if userdb._user._user-name = "" or userdb._user._user-name = ? then 
                                   if userid("userdb") <> "computos" then
                                     substring(userid("userdb"),3)
                                   else
                                     userid("userdb")
                                 else
                                    userdb._user._user-name */ "usuario"
          TTPDF.cFunction[1]   = "CalcColumn".
   if TTPDF.creporttitle = "" or TTPDF.creporttitle = ? then
     TTPDF.creporttitle = "Salida con Contenido del Browse".
/*   release userdb._user. */
   {s_pausa1.i}
   run csvlibrary.p (INPUT-OUTPUT TABLE TTPDF).
   {s_pausa2.i}
 end.

/**********************************/
/* FUNCI‡N PARA CAMPOS CALCULADOS */
/**********************************/
function CalcColumn returns character (ip_hQuery as handle, ip_iRow as int, ip_iCol as int):
  browse {&BROWSE-NAME}:query:reposition-to-row(ip_iRow).
  return browse {&BROWSE-NAME}:get-browse-column(ip_iCol):screen-value.
end function.
