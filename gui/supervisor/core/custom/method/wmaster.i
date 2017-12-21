define var lista as character no-undo.
define var i as integer no-undo.
define var h as handle no-undo.
define var lista_campos as character no-undo initial "ninguno".
define var accion as logical initial false.

if accion then
do: 
    run get-link-handle in adm-broker-hdl (input this-procedure,"CONTAINER-TARGET",
                                       output lista).
    do i=1 to num-entries(lista):
        h=widget-handle(entry(i,lista)).
        run get-attribute in h ('TYPE').
        if return-value = "smartviewer" then
            run deshabilita_campos in h (input lista_campos).
    end.
end.



