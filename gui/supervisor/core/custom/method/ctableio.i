&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*-------------------------------------------------------------------------
    File        : tableio.i  
    Purpose     : Basic ADM methods for record changes
  
    Syntax      : {src/adm/method/tableio.i}

    Description :
  
    Author(s)   :
    Created     :
    Notes       : New 8Plus Version with Multiple Enabled Table support
    HISTORY: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(adm-tableio) = 0 &THEN
&GLOBAL adm-tableio yes

/* Note: adm-<nth>-enabled-table is defined for each updatable table - 
               that is, a table with enabled fields.
         adm-tableio-first-table is the first (or only) table in the join;
               this is used for repositioning the query after an add, e.g.
         adm-tableio-fields is the list of enabled fields or 
               browse columns in all enabled tables.
         adm-tableio-table is no longer used but has been kept for
               backward compatibility with any user code which may reference it.
         In addition, we map adm-first-enabled-table to ENABLED-TABLES 
               if it's not otherwise defined, again for backward compatibility 
               with 8.0A-generated programs.  */
/* For Viewers: */
&IF DEFINED(adm-browser) = 0 &THEN
  &GLOBAL adm-tableio-table        {&ENABLED-TABLES} 

  /* Allow users to define this preproc themselves for special cases: */
  &IF DEFINED(adm-first-enabled-table) = 0 &THEN
    &IF DEFINED(FIRST-ENABLED-TABLE) = 0 &THEN
      &GLOBAL adm-first-enabled-table  {&ENABLED-TABLES}
    &ELSE
      &GLOBAL adm-first-enabled-table  {&FIRST-ENABLED-TABLE}
    &ENDIF
  &ENDIF

  &GLOBAL adm-second-enabled-table {&SECOND-ENABLED-TABLE}
  &GLOBAL adm-third-enabled-table  {&THIRD-ENABLED-TABLE}
  &IF "{&FIRST-EXTERNAL-TABLE}":U NE "":U &THEN
    &GLOBAL adm-tableio-first-table {&FIRST-EXTERNAL-TABLE}
  &ELSE
    &GLOBAL adm-tableio-first-table {&FIRST-ENABLED-TABLE}
  &ENDIF
  &GLOBAL adm-tableio-fields  {&ENABLED-FIELDS}
&ELSE
/* For Browsers: */
  &GLOBAL adm-tableio-table        {&ENABLED-TABLES-IN-QUERY-{&BROWSE-NAME}} 

  /* Allow users to define this preproc themselves for special cases: */
  &IF DEFINED(adm-first-enabled-table) = 0 &THEN
    &IF DEFINED(FIRST-ENABLED-TABLE-IN-QUERY-{&BROWSE-NAME}) = 0 &THEN
      &GLOBAL adm-first-enabled-table {&ENABLED-TABLES-IN-QUERY-{&BROWSE-NAME}}
    &ELSE
      &GLOBAL adm-first-enabled-table {&FIRST-ENABLED-TABLE-IN-QUERY-{&BROWSE-NAME}}
    &ENDIF
  &ENDIF

  &GLOBAL adm-second-enabled-table {&SECOND-ENABLED-TABLE-IN-QUERY-{&BROWSE-NAME}}
  &GLOBAL adm-third-enabled-table {&THIRD-ENABLED-TABLE-IN-QUERY-{&BROWSE-NAME}}
  &GLOBAL adm-tableio-first-table {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
  &GLOBAL adm-tableio-fields {&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}
&ENDIF

  DEFINE VARIABLE adm-first-table         AS ROWID NO-UNDO.  
  DEFINE VARIABLE adm-second-table        AS ROWID NO-UNDO. 
  DEFINE VARIABLE adm-third-table         AS ROWID NO-UNDO.
  DEFINE VARIABLE adm-adding-record       AS LOGICAL NO-UNDO INIT no.
  DEFINE VARIABLE adm-return-status       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE adm-first-prev-rowid    AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-second-prev-rowid   AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-third-prev-rowid    AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-first-add-rowid     AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-second-add-rowid    AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-third-add-rowid     AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-first-tmpl-recid    AS RECID     NO-UNDO INIT ?.
  DEFINE VARIABLE adm-second-tmpl-recid   AS RECID     NO-UNDO INIT ?.
  DEFINE VARIABLE adm-third-tmpl-recid    AS RECID     NO-UNDO INIT ?.
  DEFINE VARIABLE adm-index-pos           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE adm-query-empty         AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE adm-create-complete     AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE adm-create-on-add       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE group-assign-add        AS LOGICAL   NO-UNDO INIT ?.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 6.86
         WIDTH              = 65.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* If there are no ENABLED-TABLES, then remove the TABLEIO-TARGET link
   from the list of SUPPORTED-LINKS. */
  IF "{&adm-first-enabled-table}":U = "":U THEN
    RUN modify-list-attribute IN adm-broker-hdl
      (THIS-PROCEDURE, "REMOVE":U, "SUPPORTED-LINKS":U, "TABLEIO-TARGET":U).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-adm-add-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-add-record Method-Library 
PROCEDURE adm-add-record :
/* -----------------------------------------------------------  
      Purpose:     Initiates a record add. Displays initial values
                   but does not create the record. That is done by
                   adm-assign-record.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/  
   &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN
  
   DEFINE VARIABLE trans-hdl-string  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cntr              AS INTEGER   NO-UNDO.
   DEFINE VARIABLE temp-rowid        AS ROWID     NO-UNDO.
   DEFINE VARIABLE saved-dictdb      AS CHARACTER NO-UNDO.

      /* Check MODIFIED field attribute for the prior record. */
      RUN check-modified IN THIS-PROCEDURE ('check':U) NO-ERROR. 

             /* Save the current rowid in case the add is cancelled. */
      ASSIGN adm-first-table = ROWID({&adm-tableio-first-table})
             adm-new-record = yes     /* Signal new rec being created. */
             adm-adding-record = yes  /* Signal that it's add not copy. */
             adm-query-empty = IF AVAILABLE({&adm-tableio-first-table})
                               THEN no ELSE yes. /* needed in Cancel */
      RUN set-attribute-list ("ADM-NEW-RECORD=yes":U).


      /* If for some reason this object's fields have not been enabled from
         outside, then do it here. */
      RUN dispatch('enable-fields':U).
      &IF DEFINED(ADM-CREATE-FIELDS) NE 0 &THEN
          ENABLE {&UNLESS-HIDDEN} {&ADM-CREATE-FIELDS} WITH FRAME {&FRAME-NAME}.
      &ENDIF

      /* The first time a record is added, we determine whether this object
         is a Group-Assign-Target for another object with the same table. */
      IF group-assign-add = ? THEN
      DO: 
          RUN request-attribute IN adm-broker-hdl
            (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, 'ENABLED-TABLES':U).

          /* This will be true only if the object has a Group-Assign-Source
             *and* its first enabled table is the same as one in the G-A-S.
             If this is the case, we re-find the record the G-A-S has just
             created and add our fields to it. Otherwise we create the
             new record in this object. */
          IF LOOKUP("{&adm-first-enabled-table}":U, RETURN-VALUE) NE 0 THEN
            group-assign-add = yes.
          ELSE group-assign-add = no.
      END.

      /* The first time a record is added, we must get the RECID(s) of
         the template record(s) which hold initial values for Progress. 
         IF the Create-On-Add attribute is set to "yes", then we don't
         do this, because the CREATE will take care of initial values. 
         We do this only for Progress databases, however, since other
         DBs or Temp-Tables don't support the template record. */
      IF (adm-create-on-add = no) AND (adm-first-tmpl-recid = ?) AND
         (DBTYPE(LDBNAME(BUFFER {&adm-first-enabled-table})) EQ "PROGRESS":U)
      THEN DO:
          saved-dictdb = LDBNAME("DICTDB":U).  /* save off current DICTDB */
          /* Change the DICTDB alias to the database of each table and
             run a separate procedure to retrieve the template RECID. */
          CREATE ALIAS DICTDB FOR DATABASE 
            VALUE(LDBNAME(BUFFER {&adm-first-enabled-table})).
          RUN adm/objects/get-init.p (INPUT "{&adm-first-enabled-table}":U,
            OUTPUT adm-first-tmpl-recid).
          &IF "{&adm-second-enabled-table}":U NE "":U &THEN
            CREATE ALIAS DICTDB FOR DATABASE 
              VALUE(LDBNAME(BUFFER {&adm-second-enabled-table})).
            RUN adm/objects/get-init.p (INPUT "{&adm-second-enabled-table}":U,
              OUTPUT adm-second-tmpl-recid).
          &ENDIF
          &IF "{&adm-third-enabled-table}":U NE "":U &THEN
            CREATE ALIAS DICTDB FOR DATABASE 
              VALUE(LDBNAME(BUFFER {&adm-third-enabled-table})).
            RUN adm/objects/get-init.p (INPUT "{&adm-third-enabled-table}":U,
              OUTPUT adm-third-tmpl-recid).
          &ENDIF
          CREATE ALIAS DICTDB FOR DATABASE    /* Restore the orig. dictdb */
            VALUE(saved-dictdb).
        END.

      &IF DEFINED(adm-browser) = 0 &THEN

          IF adm-create-on-add = no THEN 
          DO:
           IF DBTYPE(LDBNAME(BUFFER {&adm-first-enabled-table})) 
             EQ "PROGRESS":U THEN
           DO:
            /* Retrieve and display the template record initial values if
               the record hasn't already been created and we're running
               against a Progress DB. */
            FIND {&adm-first-enabled-table} WHERE 
              RECID({&adm-first-enabled-table}) = adm-first-tmpl-recid.
            &IF "{&adm-second-enabled-table}":U NE "":U &THEN
              FIND {&adm-second-enabled-table} WHERE 
                RECID({&adm-second-enabled-table}) = adm-second-tmpl-recid.
            &ENDIF
            &IF "{&adm-third-enabled-table}":U NE "":U &THEN
              FIND {&adm-third-enabled-table} WHERE 
                RECID({&adm-third-enabled-table}) = adm-third-tmpl-recid.
            &ENDIF

            DISPLAY {&UNLESS-HIDDEN} {&DISPLAYED-FIELDS}
              WITH FRAME {&FRAME-NAME} NO-ERROR.
           END.
          END.              /* END code for Progress DB initial values. */
          /* If the developer explicitly requested Create-On-Add or it's
             not a Progress DB then do the CREATE. */
          ELSE DO:
           DO TRANSACTION ON STOP  UNDO, RETURN "ADM-ERROR":U 
                          ON ERROR UNDO, RETURN "ADM-ERROR":U:
             adm-create-complete = no.  /* Signal whether Create succeeded. */
             RUN dispatch ('create-record':U).
             IF RETURN-VALUE = "ADM-ERROR":U THEN UNDO, RETURN "ADM-ERROR":U.

             DISPLAY {&UNLESS-HIDDEN} {&DISPLAYED-FIELDS}
                WITH FRAME {&FRAME-NAME} NO-ERROR.

           END.
           adm-create-complete = yes.  /* Signal Cancel that Create was done. */
          END. 
      &ELSE                              /* Code for SmartBrowsers */
          DO WITH FRAME {&FRAME-NAME}:
             IF NUM-RESULTS("{&BROWSE-NAME}":U) = ? OR  /* query not opened */
              NUM-RESULTS("{&BROWSE-NAME}":U) = 0 /* query's empty */
               OR BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS = 1 THEN 
                adm-return-status = {&BROWSE-NAME}:INSERT-ROW("AFTER":U).
              ELSE DO:
                MESSAGE 
              "You must select a row after which the new row is to be inserted."
                    VIEW-AS ALERT-BOX.
                RETURN.
              END.
          /* The browser's ROW-ENTRY trigger will display initial values. 
             Set the flag which indicates that this hasn't happened yet: */
              adm-brs-initted = no.
          END.
      &ENDIF

      RUN notify ('add-record, GROUP-ASSIGN-TARGET':U).

      RUN new-state('update':U). /* Signal that we're in a record update now. */

      RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 
      
   &ELSE MESSAGE "Object ":U THIS-PROCEDURE:FILE-NAME 
       "must have at least one Enabled Table to perform Add.":U
       VIEW-AS ALERT-BOX ERROR.
   &ENDIF
 
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-assign-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-assign-record Method-Library 
PROCEDURE adm-assign-record :
/* -----------------------------------------------------------  
      Purpose:     Assigns changes to a single record as part of an
                   update, add, or copy. If this is an add or copy, 
                   the new record is created here.
      Parameters:  <none>
      Notes:       This method is intended to be invoked from 
                   adm-update-record, which starts a transaction.       
                   This allows multiple ASSIGNs in several objects 
                   (connected with the GROUP-ASSIGN link)
                   to be part of a single update transaction.
    -------------------------------------------------------------*/  

&IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN

        adm-updating-record = yes.    /* Signal row-available etc. */
        IF adm-new-record THEN DO:

          /* Unless we already did the CREATE in add-record, create the new
             record(s) for an Add here. */

          IF (NOT adm-adding-record) OR  /* Indicates Copy, not Add */
              (NOT adm-create-on-add) THEN
          DO:
             RUN dispatch ('create-record':U).
             IF RETURN-VALUE = "ADM-ERROR":U THEN UNDO, RETURN "ADM-ERROR":U.
          END.

          IF (adm-create-on-add = yes) OR (group-assign-add = yes) THEN
          DO:     /* Need to re-get the record to upgrade the lock */

           &IF DEFINED (adm-browser) NE 0 &THEN
            /* Under some circumstances the browse may lose track of the newly
               created record, so we may need to refetch it here. */
            IF adm-first-add-rowid NE ROWID({&adm-first-enabled-table}) THEN
              FIND {&adm-first-enabled-table} WHERE 
                  ROWID ({&adm-first-enabled-table}) = adm-first-add-rowid.
            &IF "{&adm-second-enabled-table}":U NE "":U &THEN
              IF adm-second-add-rowid NE ROWID({&adm-second-enabled-table}) THEN
                FIND {&adm-second-enabled-table} WHERE 
                  ROWID ({&adm-second-enabled-table}) = adm-second-add-rowid.
            &ENDIF
            &IF "{&adm-third-enabled-table}":U NE "":U &THEN
              IF adm-third-add-rowid NE ROWID({&adm-third-enabled-table}) THEN
                FIND {&adm-third-enabled-table} WHERE 
                  ROWID ({&adm-third-enabled-table}) = adm-third-add-rowid.
            &ENDIF
           &ENDIF

            RUN dispatch ('current-changed':U). /* Get an EXCLUSIVE lock */
            IF RETURN-VALUE = "ADM-ERROR":U THEN 
               RETURN "ADM-ERROR":U.
          END.
        END.       /* END of processing for Add/Copy */

        ELSE DO:   /* for ordinary ASSIGNs: */
            RUN dispatch ('current-changed':U). /* Get an EXCLUSIVE lock */
            IF RETURN-VALUE = "ADM-ERROR":U THEN 
               RETURN "ADM-ERROR":U.
        END.
        
        RUN dispatch ('assign-statement':U). /* ASSIGN the fields */
        IF RETURN-VALUE = "ADM-ERROR":U THEN 
            UNDO, RETURN "ADM-ERROR":U.
        
        /* If this object is linked to others to be updated together,
           then do that here: */
        RUN notify ('assign-record,GROUP-ASSIGN-TARGET':U).
        IF RETURN-VALUE = "ADM-ERROR":U THEN UNDO, RETURN "ADM-ERROR":U.

        /* Display any fields assigned by CREATE. */
        IF adm-new-record THEN 
        DO:
            RUN get-attribute('Query-Position':U).  /* Is this the first rec?*/
            IF RETURN-VALUE = 'no-record-available':U THEN   /* yes it is...*/
            DO:
              RUN new-state('record-available':U).  /* Let Panels know. */
              RUN set-attribute-list('Query-Position = record-available':U).
            END.
            RUN dispatch('display-fields':U).
        END.

   &ELSE MESSAGE 
       "Object ":U THIS-PROCEDURE:FILE-NAME
         "must have at least one Enabled Table to perform Assign.":U
           VIEW-AS ALERT-BOX ERROR.
   &ENDIF

   RETURN.
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-assign-statement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-assign-statement Method-Library 
PROCEDURE adm-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     ASSIGNs field values from within assign-record
  Parameters:  <none>
  Notes:       This can be replaced with a local version if the 
               ASSIGN statement needs to be customized in some way
               which is not supportable through ADM-CREATE-FIELDS
               and ADM-ASSIGN-FIELDS, for example, to assure that
               key fields which are assigned programatically don't
               force an additional database write. Also, custom
               validation can be done in local-assign-statement before
               or after the ASSIGN.
------------------------------------------------------------------------------*/
&IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 
      AND "{&adm-tableio-fields}":U NE "":U &THEN 
  &IF DEFINED(adm-browser) = 0 &THEN   /* ASSIGN for Frame Fields: */
    /* ADM-ASSIGN-FIELDS gives the developer the opportunity to define
       additional fields in the default frame which are to be assigned
       at the same time as the ENABLED-FIELDS list. */
    /* If this is a new record creation, also allow additional fields
       defined in ADM-CREATE-FIELDS to be entered. */
    &IF DEFINED(ADM-CREATE-FIELDS) NE 0 &THEN
    IF adm-new-record THEN 
    DO:
        ASSIGN FRAME {&FRAME-NAME} {&ADM-CREATE-FIELDS}
            {&adm-tableio-fields} {&ADM-ASSIGN-FIELDS} NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN   /* Leave enabled if error */
            DISABLE {&UNLESS-HIDDEN} {&ADM-CREATE-FIELDS} 
              WITH FRAME {&FRAME-NAME}.
    END.
    ELSE
    &ENDIF
        ASSIGN FRAME {&FRAME-NAME} {&adm-tableio-fields} 
             {&ADM-ASSIGN-FIELDS} NO-ERROR.

    IF ERROR-STATUS:ERROR THEN    /* DO error checking for non-browsers */
    DO: 
      RUN dispatch('show-errors':U).
      UNDO, RETURN "ADM-ERROR":U.
    END.
  &ELSE                               /* ASSIGN for Browse Fields: */
    ASSIGN BROWSE {&BROWSE-NAME} {&adm-tableio-fields} NO-ERROR.

    IF ERROR-STATUS:ERROR THEN  /* Browser gets its own error checking,   */
    DO:                         /*  only if the ASSIGN was actually done. */
      RUN dispatch('show-errors':U).
      UNDO, RETURN "ADM-ERROR":U.
    END.
  &ENDIF
&ENDIF

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-cancel-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-cancel-record Method-Library 
PROCEDURE adm-cancel-record :
/* -----------------------------------------------------------  
      Purpose:     Cancels a record update, add or copy operation
      Parameters:  <none>
      Notes:       This simply gets the query to reposition to what
                   was the current record before the add or copy began;
                   for an update of an object such as a Viewer it gets 
                   reset-record to redisplay the fields;
                   if a transaction was open for an object with its own query, 
                   such as a Browser, it reopens the query to redisplay 
                   original values for all records which may have been changed.
    -------------------------------------------------------------*/  

&IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN

  DEFINE VARIABLE source-str          AS CHARACTER NO-UNDO.

   /* Clear MODIFIED field attribute. */


   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR. 
 
   IF adm-new-record THEN           /* Reposition to prev record for add/copy */
   DO:
      /* If initial values were displayed through the use of the template
         record, then get rid of the template record.*/
      IF (adm-adding-record = yes) AND  /*  Add, not Copy */
         (adm-create-on-add = no)
      THEN DO:
        RELEASE {&adm-first-enabled-table} NO-ERROR.  
        &IF "{&adm-second-enabled-table}":U NE "":U &THEN
          RELEASE {&adm-second-enabled-table} NO-ERROR.  
        &ENDIF
        &IF "{&adm-third-enabled-table}":U NE "":U &THEN
          RELEASE {&adm-third-enabled-table} NO-ERROR.  
        &ENDIF
      END.
      /* If the actual record Create was done in add-record, then we must
         delete the record(s) to cancel. */
      ELSE IF (adm-adding-record = yes) AND  /* Add, not Copy */
        (adm-create-on-add = yes) AND
          (adm-create-complete = yes) /* Make sure rec actually created */
      THEN DO:
        &IF DEFINED (adm-browser) NE 0 &THEN
          /* Under some circumstances the browse may lose track of the newly
             created record, so we may need to refetch it here. */
          IF adm-first-add-rowid NE ROWID({&adm-first-enabled-table}) THEN
            FIND {&adm-first-enabled-table} WHERE 
                ROWID ({&adm-first-enabled-table}) = adm-first-add-rowid.
          &IF "{&adm-second-enabled-table}":U NE "":U &THEN
            IF adm-second-add-rowid NE ROWID({&adm-second-enabled-table}) THEN
              FIND {&adm-second-enabled-table} WHERE 
                ROWID ({&adm-second-enabled-table}) = adm-second-add-rowid.
          &ENDIF
          &IF "{&adm-third-enabled-table}":U NE "":U &THEN
            IF adm-third-add-rowid NE ROWID({&adm-third-enabled-table}) THEN
              FIND {&adm-third-enabled-table} WHERE 
                ROWID ({&adm-third-enabled-table}) = adm-third-add-rowid.
          &ENDIF
        &ENDIF

          RUN dispatch ('delete-record':U).
      END.
 
      &IF DEFINED(adm-browser) NE 0 &THEN   /* Delete the new browser row */
        IF (adm-adding-record = no) OR (adm-create-on-add = no) THEN
          IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS = 1 THEN 
            adm-return-status = {&BROWSE-NAME}:DELETE-CURRENT-ROW() 
             IN FRAME {&FRAME-NAME}.
      &ENDIF

      &IF DEFINED(ADM-CREATE-FIELDS) NE 0 &THEN
          DISABLE {&UNLESS-HIDDEN} {&ADM-CREATE-FIELDS} 
            WITH FRAME {&FRAME-NAME}.
      &ENDIF
      &IF DEFINED (adm-browser) = 0 &THEN
          /* For Viewers, if this was a Cancel of an Add of the only
             record in the query, then disable the fields. */
          IF adm-query-empty THEN              /* Set for us in Add */
             RUN dispatch ('disable-fields':U).
      &ENDIF
      adm-new-record = no.
      RUN set-attribute-list ("ADM-NEW-RECORD=no":U).

      RUN notify ('cancel-record, GROUP-ASSIGN-TARGET':U).
   END.
   ELSE RUN dispatch ('reset-record':U). /*For Update, redisplay old values. */

   RUN get-link-handle IN adm-broker-hdl 
       (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, OUTPUT source-str).
   IF source-str EQ "":U THEN            /* If not just a group-assign-target,*/
     RUN new-state('update-complete':U). /* tell all to reset/refresh */
   &IF DEFINED(adm-browser) NE 0 &THEN
     adm-brs-in-update = no.       /* This tells whether Update button set. */
   &ENDIF
   adm-updating-record = no.

&ENDIF

   RETURN.
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-copy-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-copy-record Method-Library 
PROCEDURE adm-copy-record :
/* -----------------------------------------------------------  
      Purpose:     Allows the creation of a new record whose initial
                   values are the same as the current record buffer.


      Parameters:  <none>
      Notes:       This is like add-record except we start with the
                   current record buffer rather than the template record.
    -------------------------------------------------------------*/  
  
   &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN
  
   DEFINE VARIABLE trans-hdl-string AS CHARACTER NO-UNDO.

      /* Check MODIFIED field attribute for the prior record. */
      RUN check-modified IN THIS-PROCEDURE ('check':U) NO-ERROR. 

             /* Save the current rowid in case the copy is cancelled. */
      ASSIGN adm-first-table = ROWID({&adm-tableio-first-table})
             adm-new-record = yes     /* Signal a new rec is being created. */
             adm-adding-record = no.  /* Signal this is copy not add. */
      RUN set-attribute-list ("ADM-NEW-RECORD=yes":U).

      /* If for some reason this object's fields have not been enabled from
         outside, then do it here. */
      RUN dispatch('enable-fields':U).
      &IF DEFINED(ADM-CREATE-FIELDS) NE 0 &THEN
          ENABLE {&UNLESS-HIDDEN} {&ADM-CREATE-FIELDS} WITH FRAME {&FRAME-NAME}.
      &ENDIF

      /* The first time a record is added or copied, we determine whether 
         this object is a Group-Assign-Target for another object with the same 
         table. */
      IF group-assign-add = ? THEN
      DO: 
          RUN request-attribute IN adm-broker-hdl
            (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, 'ENABLED-TABLES':U).

          /* This will be true only if the object has a Group-Assign-Source
             *and* its first enabled table is the same as one in the G-A-S.
             If this is the case, we re-find the record the G-A-S has just
             created and add our fields to it. Otherwise we create the
             new record in this object. */
          IF LOOKUP("{&adm-first-enabled-table}":U, RETURN-VALUE) NE 0 THEN
            group-assign-add = yes.
          ELSE group-assign-add = no.
      END.

      &IF DEFINED(adm-browser) = 0 &THEN
          RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 
      &ELSE
          DO WITH FRAME {&FRAME-NAME}:
            IF NUM-RESULTS("{&BROWSE-NAME}":U) = ? OR  /* query's not opened */
               NUM-RESULTS("{&BROWSE-NAME}":U) = 0 THEN /* query's empty */
            DO:
                MESSAGE "Cannot perform Copy. There are no browse rows."
                    VIEW-AS ALERT-BOX WARNING.
                RUN dispatch ('cancel-record':U).
            END.
            ELSE DO:
             adm-first-prev-rowid = ROWID({&adm-first-enabled-table}). 
             &IF "{&adm-second-enabled-table}":U NE "":U &THEN
               adm-second-prev-rowid = ROWID({&adm-second-enabled-table}). 
             &ENDIF
             &IF "{&adm-third-enabled-table}":U NE "":U &THEN
               adm-third-prev-rowid = ROWID({&adm-third-enabled-table}). 
             &ENDIF
               IF NUM-RESULTS("{&BROWSE-NAME}":U) = ? OR  /* query not opened */
                NUM-RESULTS("{&BROWSE-NAME}":U) = 0 /* query's empty */
                 OR BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS = 1 THEN 
                  adm-return-status = {&BROWSE-NAME}:INSERT-ROW("AFTER":U).
               ELSE DO:
                  MESSAGE 
              "You must select a row after which the new row is to be inserted."
                      VIEW-AS ALERT-BOX.
                  RETURN.
               END.
               /* The browser's ROW-ENTRY trigger will display initial values. 
                  Set the flag which indicates that this hasn't happened yet: */
               adm-brs-initted = no.
            END.
          END.
      &ENDIF

      RUN notify ('copy-record, GROUP-ASSIGN-TARGET':U).

      RUN new-state('update':U). /* Signal that we're in a record update now. */
   
   &ELSE MESSAGE "Object ":U THIS-PROCEDURE:FILE-NAME 
     "must have at least one Enabled Table to perform Copy.":U
       VIEW-AS ALERT-BOX ERROR.
   &ENDIF
 
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-create-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-record Method-Library 
PROCEDURE adm-create-record :
/* -----------------------------------------------------------  
      Purpose:    Performs the actual CREATE <table> statement(s)
                  to create a new row for a query. This is normally
                  dispatched from adm-assign-record, but may be done 
                  from adm-add-record if the 'Create-On'Add' attribute
                  is set.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/  
   &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN

    DEFINE VARIABLE source-str          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE source-rowid-str    AS CHARACTER NO-UNDO.

       /* If this is a Group-Assign, then the first (or only) enabled
          table will be passed on from its parent. Any others will be 
          created here. */

       IF group-assign-add = yes THEN
       DO:
         RUN get-link-handle IN adm-broker-hdl 
           (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, 
              OUTPUT source-str).
         RUN send-records IN WIDGET-HANDLE (source-str)
             (INPUT "{&adm-first-enabled-table}":U, 
              OUTPUT source-rowid-str).
         FIND {&adm-first-enabled-table} WHERE 
             ROWID ({&adm-first-enabled-table}) = 
                 TO-ROWID(source-rowid-str) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO: 
           RUN dispatch('show-errors':U).
           UNDO, RETURN "ADM-ERROR":U.
         END.
       END.
       ELSE DO:
           CREATE {&adm-first-enabled-table} NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
           DO: 
             RUN dispatch('show-errors':U).
             UNDO, RETURN "ADM-ERROR":U.
           END.
       END.
   
       /* Save off the ROWID(s) of the new record(s) because under some
       circumstances, a browse may lose track of the record and it needs
       to be re-retrieved in assign-record. */

       adm-first-add-rowid = ROWID({&adm-first-enabled-table}).

       &IF "{&adm-second-enabled-table}":U NE "":U &THEN
         CREATE {&adm-second-enabled-table} NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO: 
           RUN dispatch('show-errors':U).
           UNDO, RETURN "ADM-ERROR":U.
         END.
       adm-second-add-rowid = ROWID({&adm-second-enabled-table}).
       &ENDIF

       &IF "{&adm-third-enabled-table}":U NE "":U &THEN
         CREATE {&adm-third-enabled-table} NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO: 
           RUN dispatch('show-errors':U).
           UNDO, RETURN "ADM-ERROR":U.
         END.
       adm-third-add-rowid = ROWID({&adm-third-enabled-table}).
       &ENDIF
   &ENDIF
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-current-changed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-current-changed Method-Library 
PROCEDURE adm-current-changed :
/*------------------------------------------------------------------------------
  Purpose:     Upgrades the lock on the current record to EXCLUSIVE.
               Checks whether it has been changed and redisplays
               the values in the changed record if it has been changed.
  Parameters:  <none>
  Notes:       Can be customized to change the lock upgrade code or
               to replace or supplement the CURRENT-CHANGED function,
               for example, to save off this user's changes and
               reconcile them with the other copy of the record.
------------------------------------------------------------------------------*/
&IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN

   /* Save the ROWID of the current record in case the lock upgrade fails. */
   ASSIGN adm-first-table = ROWID({&adm-first-enabled-table}).
   &IF "{&adm-second-enabled-table}":U NE "":U &THEN
     ASSIGN adm-second-table = ROWID({&adm-second-enabled-table}).
   &ENDIF
   &IF "{&adm-third-enabled-table}":U NE "":U &THEN
     ASSIGN adm-third-table = ROWID({&adm-third-enabled-table}).
   &ENDIF
   
   /* Note that we do the FIND EXCLUSIVE-LOCK and CURRENT-CHANGED without
           checking the initial lock state first. This is because it is not
           possible to be certain what the actual lock state is at this time,
           and the check is not expensive if in fact the record was already
           locked. */
  FIND CURRENT {&adm-first-enabled-table} EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE {&adm-first-enabled-table} THEN DO:
      RUN dispatch('show-errors':U).
      /* A Record Not On File error means the record has been deleted */
      IF ERROR-STATUS:GET-NUMBER(1) = 138 THEN
          RUN dispatch('get-next':U).      /* get off this record. */
      ELSE FIND {&adm-first-enabled-table} WHERE 
          ROWID({&adm-first-enabled-table}) = adm-first-table NO-LOCK NO-ERROR.  
      RETURN "ADM-ERROR":U.
  END.
  ELSE IF CURRENT-CHANGED {&adm-first-enabled-table} THEN DO:
      /* Error processing if someone else has changed the record. */
      MESSAGE  SUBSTITUTE
          ("Sorry, this &1 has been changed by another user. ",
            "{&adm-first-enabled-table}") SKIP
            "Please note any differences and re-enter your changes."
                   VIEW-AS ALERT-BOX.
      RUN dispatch ('display-fields':U).
      UNDO, RETURN "ADM-ERROR":U.    
  END.
&IF "{&adm-second-enabled-table}":U NE "":U &THEN
  FIND CURRENT {&adm-second-enabled-table} EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE {&adm-second-enabled-table} THEN DO:
      RUN dispatch('show-errors':U).
      /* A Record Not On File error means the record has been deleted */
      IF ERROR-STATUS:GET-NUMBER(1) = 138 THEN
          RUN dispatch('get-next':U).      /* get off this record. */
      ELSE FIND {&adm-second-enabled-table} WHERE 
          ROWID({&adm-second-enabled-table}) = adm-second-table NO-LOCK NO-ERROR.  
      RETURN "ADM-ERROR":U.
  END.
  ELSE IF CURRENT-CHANGED {&adm-second-enabled-table} THEN DO:
      /* Error processing if someone else has changed the record. */
      MESSAGE  SUBSTITUTE
          ("Sorry, this &1 has been changed by another user. ",
            "{&adm-second-enabled-table}") SKIP
            "Please note any differences and re-enter your changes."
                   VIEW-AS ALERT-BOX.
      RUN dispatch ('display-fields':U).
      UNDO, RETURN "ADM-ERROR":U.    
  END.
&ENDIF
&IF "{&adm-third-enabled-table}":U NE "":U &THEN
  FIND CURRENT {&adm-third-enabled-table} EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE {&adm-third-enabled-table} THEN DO:
      RUN dispatch('show-errors':U).
      /* A Record Not On File error means the record has been deleted */
      IF ERROR-STATUS:GET-NUMBER(1) = 138 THEN
          RUN dispatch('get-next':U).      /* get off this record. */
      ELSE FIND {&adm-third-enabled-table} WHERE 
          ROWID({&adm-third-enabled-table}) = adm-third-table NO-LOCK NO-ERROR.  
      RETURN "ADM-ERROR":U.
  END.
  ELSE IF CURRENT-CHANGED {&adm-third-enabled-table} THEN DO:
      /* Error processing if someone else has changed the record. */
      MESSAGE  SUBSTITUTE
          ("Sorry, this &1 has been changed by another user. ",
            "{&adm-third-enabled-table}") SKIP
            "Please note any differences and re-enter your changes."
                   VIEW-AS ALERT-BOX.
      RUN dispatch ('display-fields':U).
      UNDO, RETURN "ADM-ERROR":U.    
  END.
&ENDIF
&ENDIF
      
  RETURN.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-delete-record Method-Library 
PROCEDURE adm-delete-record :
/* -----------------------------------------------------------
      Purpose:     Deletes the current record.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/           
        
&IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN

   DEFINE VARIABLE delete-failed AS LOGICAL NO-UNDO INIT no.
      
   &IF DEFINED(adm-browser) NE 0 &THEN
      IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS NE 1 THEN 
      DO:
          MESSAGE "No row has been selected for deletion." VIEW-AS ALERT-BOX.
          RETURN.
      END.
   &ENDIF

      DO TRANSACTION ON STOP UNDO, LEAVE ON ERROR UNDO, LEAVE:
      
        /* If this object has a group-assign-source for its first or only
           table, then don't try to re-delete the record here. */
        IF group-assign-add NE yes THEN
        DO:
          FIND CURRENT {&adm-first-enabled-table} EXCLUSIVE-LOCK NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
          DO: 
            RUN dispatch('show-errors':U).
            UNDO, RETURN "ADM-ERROR":U.
          END.
          DELETE {&adm-first-enabled-table} NO-ERROR.
        END.

        &IF "{&adm-second-enabled-table}":U NE "":U &THEN
          /* If the first delete failed then skip any other tables. */
          IF NOT ERROR-STATUS:ERROR THEN
          DO:
            FIND CURRENT {&adm-second-enabled-table} EXCLUSIVE-LOCK NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO: 
              RUN dispatch('show-errors':U).
              UNDO, RETURN "ADM-ERROR":U.
            END.
            DELETE {&adm-second-enabled-table} NO-ERROR.
          END.
        &ENDIF
        &IF "{&adm-third-enabled-table}":U NE "":U &THEN
          IF NOT ERROR-STATUS:ERROR THEN
          DO:
            FIND CURRENT {&adm-third-enabled-table} EXCLUSIVE-LOCK NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO: 
              RUN dispatch('show-errors':U).
              UNDO, RETURN "ADM-ERROR":U.
            END.
            DELETE {&adm-third-enabled-table} NO-ERROR.
          END.
        &ENDIF
    
        IF ERROR-STATUS:ERROR THEN
        DO: 
          RUN dispatch('show-errors':U).
          delete-failed = yes.
        END.    
        &IF DEFINED(adm-browser) NE 0 &THEN
        ELSE
            adm-return-status = 
               {&BROWSE-NAME}:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME}.
        &ENDIF
      END.

      &IF DEFINED(adm-open-query) = 0 &THEN  /* If I don't have my own query, */
         IF not delete-failed THEN           /*  if the delete succeeded, then*/
         RUN new-state ('delete-complete':U). /* get query object to sync up. */
      &ELSE

         /* If I *do* have my own query, check to see if the last record
            in the query was deleted.  */
         IF NOT AVAILABLE ({&adm-first-enabled-table}) THEN
         DO:
           RUN new-state ('no-record-available':U). 
           RUN set-attribute-list ('Query-Position = no-record-available':U).
           RUN notify ('row-available':U).  /* Tell dependent objects to clear*/
         END.
      &ENDIF

      IF delete-failed THEN
          UNDO, RETURN "ADM-ERROR":U.

   &ELSE MESSAGE 
       "Object ":U THIS-PROCEDURE:FILE-NAME 
         "must have at least one Enabled Table to perform Delete.":U
           VIEW-AS ALERT-BOX ERROR.
   &ENDIF              

    RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 
    
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-disable-fields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-disable-fields Method-Library 
PROCEDURE adm-disable-fields :
/* -----------------------------------------------------------
      Purpose:     Disables fields in the {&ENABLED-FIELDS} list.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/           
        
   &IF "{&adm-tableio-fields}":U NE "":U &THEN
     &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN
       &IF DEFINED(adm-browser) NE 0 &THEN  
           /* Get focus out of the browser and then make it read-only. */
           RUN notify ('apply-entry, TABLEIO-SOURCE':U).
           {&BROWSE-NAME}:READ-ONLY IN FRAME {&FRAME-NAME} = yes.
       &ELSE
           DISABLE {&UNLESS-HIDDEN} {&adm-tableio-fields} 
             WITH FRAME {&FRAME-NAME}.
           RUN set-editors('DISABLE':U).    /* Adjust editor widgets */
       &ENDIF
       RUN set-attribute-list ("FIELDS-ENABLED=no":U).
     &ELSE MESSAGE 
       "Object ":U THIS-PROCEDURE:FILE-NAME 
         "must have at least one Enabled Table to perform Disable-Fields.":U
           VIEW-AS ALERT-BOX ERROR.
     &ENDIF
   &ENDIF
      /* If this object is linked to others to be updated together,
         then disable fields together: */
      RUN notify ('disable-fields, GROUP-ASSIGN-TARGET':U).
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-enable-fields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-enable-fields Method-Library 
PROCEDURE adm-enable-fields :
/* -----------------------------------------------------------
      Purpose:     Enable all db fields in the {&ENABLED-FIELDS} list
                   for the default frame. Refind the current record
                   SHARE-LOCKED, and redisplay it in case it has changed.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/           
        
   &IF "{&adm-tableio-fields}":U NE "":U &THEN
     &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN
        RUN get-attribute ("FIELDS-ENABLED":U).
        IF RETURN-VALUE NE "YES":U THEN   /* Skip everything if fields are */
        DO:                               /*  already enabled. */
            IF AVAILABLE({&adm-first-enabled-table}) AND
              adm-initial-lock = "SHARE-LOCK":U OR
                adm-initial-lock = "EXCLUSIVE-LOCK":U THEN  
            DO:                                    /* +++ New code for EXCL */ 
                IF adm-initial-lock = "SHARE-LOCK":U THEN
                  FIND CURRENT {&adm-first-enabled-table} SHARE-LOCK NO-ERROR.
                /* For an EXCLUSIVE-LOCK, get the lock momentarily just to
                   make sure no-one else can deadlock with you. Then downgrade
                   back to share-lock until the record is saved. */
                ELSE IF AVAILABLE ({&adm-first-enabled-table}) THEN
                DO TRANSACTION:
                  FIND CURRENT {&adm-first-enabled-table} 
                   EXCLUSIVE-LOCK NO-ERROR.
                END.
                IF ERROR-STATUS:ERROR THEN
                DO: 
                  RUN dispatch('show-errors':U).
                  UNDO, RETURN "ADM-ERROR":U.
                END.
                RUN dispatch ('display-fields':U).  /* reshow in case changed.*/
            END.
            &IF "{&adm-second-enabled-table}":U NE "":U &THEN
              IF AVAILABLE({&adm-second-enabled-table}) AND
                adm-initial-lock = "SHARE-LOCK":U OR
                  adm-initial-lock = "EXCLUSIVE-LOCK":U THEN 
              DO:
                  IF adm-initial-lock = "SHARE-LOCK":U THEN
                    FIND CURRENT {&adm-second-enabled-table} 
                      SHARE-LOCK NO-ERROR.
                  ELSE IF AVAILABLE ({&adm-second-enabled-table}) THEN
                  DO TRANSACTION:
                    FIND CURRENT {&adm-second-enabled-table} 
                      EXCLUSIVE-LOCK NO-ERROR.
                  END.
                
                  IF ERROR-STATUS:ERROR THEN
                  DO: 
                    RUN dispatch('show-errors':U).
                    UNDO, RETURN "ADM-ERROR":U.
                  END.
                  RUN dispatch ('display-fields':U). /* reshow in case changed*/
              END.
            &ENDIF
            &IF "{&adm-third-enabled-table}":U NE "":U &THEN
              IF AVAILABLE({&adm-third-enabled-table}) AND
                adm-initial-lock = "SHARE-LOCK":U OR 
                  adm-initial-lock = "EXCLUSIVE-LOCK":U THEN 
              DO:
                  IF adm-initial-lock = "SHARE-LOCK":U THEN
                    FIND CURRENT {&adm-third-enabled-table} 
                      SHARE-LOCK NO-ERROR.
                  ELSE IF AVAILABLE ({&adm-third-enabled-table}) THEN
                  DO TRANSACTION:
                    FIND CURRENT {&adm-third-enabled-table} 
                      EXCLUSIVE-LOCK NO-ERROR.
                  END.
                  IF ERROR-STATUS:ERROR THEN
                  DO: 
                    RUN dispatch('show-errors':U).
                    UNDO, RETURN "ADM-ERROR":U.
                  END.
                  RUN dispatch ('display-fields':U). /* reshow in case changed*/
              END.
            &ENDIF

            &IF DEFINED(adm-browser) NE 0 &THEN  
              DO WITH FRAME {&FRAME-NAME}:
                DEFINE VARIABLE row-is-selected AS LOGICAL NO-UNDO.
                row-is-selected = 
                  BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS = 1.
                /* Get focus out of the browser and then make it read-only. */
                RUN notify ('apply-entry, TABLEIO-SOURCE':U).
                {&BROWSE-NAME}:READ-ONLY = no.
                /* Turning read-only off deselects the current row,
                   so we must reselect it. */
                IF row-is-selected THEN
                    adm-return-status = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
              END.
            &ELSE
                ENABLE {&UNLESS-HIDDEN} {&adm-tableio-fields} 
                  WITH FRAME {&FRAME-NAME}.
                RUN set-editors('ENABLE':U).    /* Adjust editor widgets */
            &ENDIF
            RUN set-attribute-list ("FIELDS-ENABLED=yes":U).
        END.
     &ELSE MESSAGE 
       "Object ":U THIS-PROCEDURE:FILE-NAME 
         "must have at least one Enabled Table to perform Enable-Fields.":U
           VIEW-AS ALERT-BOX ERROR.
     &ENDIF
        /* If this object is linked to others to be updated together,
           then enable fields together: */
        RUN notify ('enable-fields, GROUP-ASSIGN-TARGET':U).
        
        RUN dispatch ('apply-entry':U). /*  Assure focus is in this object. */
   &ENDIF

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-end-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-end-update Method-Library 
PROCEDURE adm-end-update :
/*------------------------------------------------------------------------------
  Purpose:   Does final update processing, including reopening the query
             on an add so that the new record becomes part of the query,
             and notifying others that a record has changed and that the 
             update is complete.  
  Parameters:  <none>
  Notes:     This is dispatched from adm-update-record if there is no 
             larger transaction active. Otherwise (with the Transaction
             Update Panel, for example) it must be invoked after the
             transaction is complete, because otherwise the query re-open
             may fail on some DataServers. 
------------------------------------------------------------------------------*/
   
  &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN

  DEFINE VARIABLE source-str          AS CHARACTER NO-UNDO.
  
   /* Give any final validation errors a chance to be seen and intercepted. */
   IF ERROR-STATUS:ERROR THEN
       RUN dispatch('show-errors':U).

   /* Clear MODIFIED field attr. */
   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.  

   IF adm-new-record THEN DO:
          adm-new-record = no.
          RUN set-attribute-list ("ADM-NEW-RECORD=no":U).

          /* Save the new rowid so it will be repositioned to. */
          ASSIGN adm-first-table = ROWID({&adm-tableio-first-table}).
  
          /* Get the record source, if any, to redisplay the changed record.
             First tell it that a reposition-query is coming so that it
             will suppress the intervening get-first event. */
          &IF DEFINED(adm-open-query) = 0 &THEN
              RUN set-link-attribute IN adm-broker-hdl
                  (THIS-PROCEDURE, 'RECORD-SOURCE':U, 
                   'REPOSITION-PENDING=yes':U).
              RUN notify('open-query':U).
              RUN request IN adm-broker-hdl (INPUT THIS-PROCEDURE,
                  INPUT 'RECORD-SOURCE':U, INPUT 'reposition-query':U).
          &ELSE                  /* record source is local - reopen if new */
              RUN set-attribute-list ('REPOSITION-PENDING=yes':U).
              RUN dispatch('open-query':U).
              RUN reposition-query (INPUT THIS-PROCEDURE).
          &ENDIF
   END.
 
    /* Release the lock in case others are waiting for this record. */
    FIND CURRENT {&adm-first-enabled-table} NO-LOCK NO-ERROR.
    &IF "{&adm-second-enabled-table}":U NE "":U &THEN
      FIND CURRENT {&adm-second-enabled-table} NO-LOCK NO-ERROR.
    &ENDIF
    &IF "{&adm-third-enabled-table}":U NE "":U &THEN
      FIND CURRENT {&adm-third-enabled-table} NO-LOCK NO-ERROR.
    &ENDIF

   /* Tell any other objects in the transaction to clear themselves too. */
    RUN notify('end-update, GROUP-ASSIGN-TARGET':U).
  
  /* Signal query and its dependents, etc. to refresh and reset themselves. 
     Only do this, however, if we're not just a Group-Assign-Target
     of some other object; in that case, let it send the message. */
    RUN get-link-handle IN adm-broker-hdl 
        (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, OUTPUT source-str).
    IF source-str EQ "":U THEN
        RUN new-state('update-complete':U). /*Tell all to reset/refresh */
    &IF DEFINED(adm-browser) NE 0 &THEN
      adm-brs-in-update = no.      /* This tells whether Update button set. */
    &ENDIF
    adm-updating-record = no.
    RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 
    
  &ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-reset-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-reset-record Method-Library 
PROCEDURE adm-reset-record :
/* -----------------------------------------------------------  
      Purpose:     Redisplays values from the record buffer for the
                   current record.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/  
  &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN
     
     RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

     /* If this object is linked to others to be updated together,
        then reset all records: */
     RUN notify ('reset-record, GROUP-ASSIGN-TARGET':U).
     RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 

  &ENDIF
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-update-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-update-record Method-Library 
PROCEDURE adm-update-record :
/* -----------------------------------------------------------  
      Purpose:     Defines a transaction within which assign-record
                   commits changes to the current record. 
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    
   &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN

      DO TRANSACTION ON STOP  UNDO, RETURN "ADM-ERROR":U 
                     ON ERROR UNDO, RETURN "ADM-ERROR":U :
        RUN dispatch ('assign-record':U).

        IF  RETURN-VALUE = "ADM-ERROR":U THEN
            RETURN "ADM-ERROR":U.
      END.
      
      /* Do final update processing, unless there is a larger transaction
         open elsewhere, in which case it must be done when the
         transaction is ended. */
       
      IF NOT TRANSACTION THEN
          RUN dispatch ('end-update':U).
   
    &ELSE MESSAGE 
      "Object ":U THIS-PROCEDURE:FILE-NAME 
        "must have at least one Enabled Table to perform Update.":U
          VIEW-AS ALERT-BOX ERROR.
    &ENDIF              

   
    RETURN.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-modified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-modified Method-Library 
PROCEDURE check-modified :
/*------------------------------------------------------------------------------
  Purpose:     Either checks or clears the MODIFIED attribute of
               all the enabled widgets in this object. Done as part
               protecting users from losing updates in the record
               changes or the application exits.
  Parameters:  <none>
  Notes:       The code checks first to make sure the FRAME or BROWSE
               hasn't already been destroyed, and that the changed record
               is still available.
               If the CHECK-MODIFIED-ALL attribute is set to "YES"
               (maps to local variable adm-check-modified-all) then
               all fields will be checked; otherwise, by default,
               only enabled database record fields are checked.
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER check-state AS CHARACTER NO-UNDO.

DEFINE VARIABLE curr-widget       AS HANDLE      NO-UNDO.
DEFINE VARIABLE container-hdl-str AS CHARACTER   NO-UNDO.

&IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN

IF NOT VALID-HANDLE(adm-object-hdl) THEN RETURN. /* Has object been destroyed?*/

&IF DEFINED(adm-browser) = 0 &THEN
  IF VALID-HANDLE(FRAME {&FRAME-NAME}:HANDLE) AND 
      AVAILABLE({&adm-first-enabled-table}) THEN
  DO:
    ASSIGN curr-widget = FRAME {&FRAME-NAME}:FIRST-CHILD. /* Field group */
    ASSIGN curr-widget = curr-widget:FIRST-CHILD. /* First field */
    DO WHILE VALID-HANDLE (curr-widget):
        IF LOOKUP (curr-widget:TYPE, 
        "FILL-IN,COMBO-BOX,EDITOR,RADIO-SET,SELECTION-LIST,SLIDER,TOGGLE-BOX":U)
            NE 0 AND 
                 /* check ENABLED fields only unless attribute says otherwise */
                 (adm-check-modified-all = yes OR curr-widget:SENSITIVE) AND
                 /* check db fields only unless attribute says otherwise */
                 (adm-check-modified-all = yes OR curr-widget:TABLE NE ?) 
                 AND curr-widget:MODIFIED THEN 
&ELSE                    /* Code for Browsers */
  IF VALID-HANDLE(BROWSE {&BROWSE-NAME}:HANDLE) AND
      AVAILABLE({&adm-first-enabled-table}) THEN
  DO:
    ASSIGN curr-widget = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    DO WHILE VALID-HANDLE (curr-widget):
        IF NOT curr-widget:READ-ONLY AND curr-widget:MODIFIED THEN
&ENDIF
        DO:
            IF check-state = "check":U THEN /*Check for changes before leaving*/
            DO:
                /* If the Container has been hidden (for destroy, e.g.), 
                   force it to view again. */
                RUN request-attribute IN adm-broker-hdl (THIS-PROCEDURE,
                    'CONTAINER-SOURCE':U, 'HIDDEN':U).
                IF RETURN-VALUE = "YES":U THEN
                    RUN notify ('view,CONTAINER-SOURCE':U).
                MESSAGE IF 
&IF DEFINED(adm-browser) = 0 OR PROVERSION GE "8.2" &THEN
                           curr-widget:TABLE NE ? 
&ELSE
                           false   /* TABLE not queryable for Browsers in 8.1 */
&ENDIF
                                 THEN 
                  SUBSTITUTE ("Current &1 record has been changed.",
                    curr-widget:TABLE) 
                  ELSE "Current values have been changed."
                  SKIP "  Do you wish to save those changes?" 
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                      UPDATE ANS AS LOGICAL.
               IF ANS THEN 
               DO:
                   RUN dispatch('update-record':U).
                   IF RETURN-VALUE = "ADM-ERROR":U THEN
                   DO:
                       MESSAGE "Changes to the previous record were not saved."
                           VIEW-AS ALERT-BOX ERROR.
                       RUN dispatch ('cancel-record':U). /* Reset all states. */
                   END.
               END.
               ELSE RUN dispatch('cancel-record':U).
               RETURN.
            END.
            ELSE IF check-state = "clear":U THEN
                curr-widget:MODIFIED = no.
        END.
&IF DEFINED(adm-browser) = 0 &THEN
        ASSIGN curr-widget = curr-widget:NEXT-SIBLING.
&ELSE
        ASSIGN curr-widget = curr-widget:NEXT-COLUMN.
&ENDIF
    END.
  END.
&ENDIF
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-rowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid Method-Library 
PROCEDURE get-rowid :
/* -----------------------------------------------------------
      Purpose:      Furnishes the rowid of the current record
                    or "previously current" record (in the event
                    of a cancelled Add or Copy, for example) to a requesting
                    procedure (typically reposition-query).
                    Note that the rowid is saved only for certain update
                    operations, in order to allow repositioning after the
                    update is complete or has been cancelled. get-rowid 
                    should not be used as a general way to get the ROWID
                    of the current record. send-records should be used instead.
      Parameters:   OUTPUT record rowid.
      Notes:
    -------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER p-table           AS ROWID NO-UNDO.

    ASSIGN
    p-table   =   adm-first-table.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-add-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record Method-Library 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.
run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
h=widget-handle(ch).
  

  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h) then
    run pre-create in h (input this-procedure).   
  run dispatch in this-procedure (input 'pre-create').
  
  run get-attribute ('modo-insert').
  if return-value = "final" then
    run dispatch IN THIS-PROCEDURE ( INPUT 'get-last').
  /* Dispatch standard ADM method. 
                              */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(h) then
    run post-create in h (input this-procedure).   
  run dispatch in this-procedure(input 'post-create').  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-delete-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record Method-Library 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.

run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
h=widget-handle(ch).
run get-rowid(output r).

  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h) then
    run pre-delete in h (input r , input this-procedure). 
  run dispatch in this-procedure (input 'pre-delete').    
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(h) then
    run post-delete in h (input this-procedure).  
  run dispatch in this-procedure (input 'post-delete').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-end-update) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-end-update Method-Library 
PROCEDURE local-end-update :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
      
   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'end-update':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
  h=widget-handle(ch).
  if valid-handle(h) then
  do:
    run get-rowid in this-procedure (output r).
    run post-update in h (input r , input this-procedure).   
  end.  
  run dispatch in this-procedure (input 'post-update'). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-local-update-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record Method-Library 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
define var h as handle no-undo.
define var ch as character no-undo.
define var r as rowid no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
    run get-link-handle in adm-broker-hdl (input THIS-PROCEDURE, input "CONTAINER-SOURCE", output ch).
    h=widget-handle(ch).
  

  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h) then
    run pre-update in h (input this-procedure).   
  run dispatch in this-procedure (input 'pre-update').
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(h) then
    run post-update in h (input this-procedure).   
  run dispatch in this-procedure(input 'post-update').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-editors) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-editors Method-Library 
PROCEDURE set-editors :
/* -----------------------------------------------------------
      Purpose:      Set the attributes of editor widgets properly.
                    They must be SENSITIVE AND READ-ONLY if disabled,
                    ELSE SENSITIVE AND not READ-ONLY. Otherwise they will
                    be unscrollable and possibly the text will be invisible
                    when they are disabled. Also used (by Add) to
                    clear any non-enabled editors (since we can't yet display
                    initial values into non-fill-ins).
      Parameters:   INPUT field-setting ("INITIALIZE" OR 
                    "ENABLED" or "DISABLED" or "CLEAR").
      Notes:        The checks are made only for editor widgets which are
                    mapped to database fields. In addition, a list is built
                    during 'initialize' of any editors whose initial state
                    is READ-ONLY, so that these are not enabled later on.
    -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-field-setting  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE curr-widget             AS HANDLE    NO-UNDO.
    DEFINE VARIABLE read-only-list          AS CHARACTER NO-UNDO INIT "":U.

    ASSIGN curr-widget = FRAME {&FRAME-NAME}:CURRENT-ITERATION. /* Field group*/
    ASSIGN curr-widget = curr-widget:FIRST-CHILD. /* First field */
    DO WHILE VALID-HANDLE (curr-widget):
        IF curr-widget:TYPE = "EDITOR":U AND curr-widget:TABLE NE ? AND
           curr-widget:HIDDEN = no THEN DO:
          CASE p-field-setting:
            WHEN "INITIALIZE":U THEN
            /* If any editor widgets have been marked as READ-ONLY then
               put them into an attribute list and leave them alone later. */
            DO:
              IF curr-widget:READ-ONLY = yes THEN read-only-list =
                  read-only-list + 
                    (IF read-only-list NE "":U THEN ",":U ELSE "":U) +
                     STRING(curr-widget).
            END.
            WHEN "DISABLE":U OR
            WHEN "ENABLE":U THEN
            DO:
                curr-widget:SENSITIVE = yes.  /* ALlow scrolling in any case.*/
                RUN get-attribute ('Read-Only-Editors':U).
                IF RETURN-VALUE = ? OR
                  LOOKUP (STRING(curr-widget), RETURN-VALUE) EQ 0 THEN 
                    curr-widget:READ-ONLY = 
                      IF p-field-setting = "ENABLE":U THEN no ELSE yes.
            END.
            WHEN "CLEAR":U THEN
                curr-widget:SCREEN-VALUE = "":U.    /* Clear for Add */
          END CASE.
        END.
        ASSIGN curr-widget = curr-widget:NEXT-SIBLING.
    END.
 
    IF p-field-setting = "INITIALIZE":U AND read-only-list NE "":U THEN
      RUN set-attribute-list ('Read-Only-Editors = "':U + read-only-list 
        + '"':U).

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-use-check-modified-all) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE use-check-modified-all Method-Library 
PROCEDURE use-check-modified-all :
/*------------------------------------------------------------------------------
  Purpose:     Sets a variable whenever the 'Check-Modified-All'
               attribute is set, indicating whether the check-modified
               procedure should check just fields which are in database records
               (the default) or all enabled fields (Check-modified-all = yes).
  Parameters:  attribute value - "YES" means check all enabled fields.
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER p-attr-value AS CHARACTER NO-UNDO.
  
  ASSIGN adm-check-modified-all = IF p-attr-value = "YES":U THEN yes ELSE no.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-use-create-on-add) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE use-create-on-add Method-Library 
PROCEDURE use-create-on-add :
/*------------------------------------------------------------------------------
  Purpose:     Stores the value of the Create-On-Add attribute whenever
               it is set.
  Parameters:  attribute value
  Notes:       This attribute tells whether the developer wants a CREATE done
               when the Add button is pressed. 
               The default for Progress DBs is no -
               the Create is done when the record is Saved. 
               The default for non-Progress DBs is yes, because 
               there is no other way to display initial values properly. 
               Because the default (Unknown) value thus can be 
               interpreted differently depending on the source of the table,
               the adm-create-on-add variable is set explicitly to yes or no. 
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-attr-value AS CHARACTER NO-UNDO.
  
   &IF NUM-ENTRIES("{&adm-first-enabled-table}":U, " ":U) = 1 &THEN
      ASSIGN adm-create-on-add = 
          IF (p-attr-value EQ "NO":U) OR
             (p-attr-value NE "YES":U AND
           DBTYPE(LDBNAME(BUFFER {&adm-first-enabled-table})) EQ "PROGRESS":U)
          THEN no ELSE yes.
   &ENDIF
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-use-initial-lock) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE use-initial-lock Method-Library 
PROCEDURE use-initial-lock :
/*------------------------------------------------------------------------------
  Purpose:   Sets the local variable adm-initial-lock whenever the
             INITIAL-LOCK attribute is set for an object. This preserves
             compatibility with code in 8.0A which looks at adm-initial-lock,
             and saves the overhead of running get-attribute('INITIAL-LOCK')
             every time a record is read, since this attribute is normally
             set only once, during initialization.  
  Parameters:  attribute value: NO-LOCK, SHARE-LOCK, or EXCLUSIVE-LOCK
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER p-attr-value AS CHARACTER NO-UNDO.
  
  ASSIGN adm-initial-lock = p-attr-value.
  
END PROCEDURE.

&ENDIF                          /* end of &IF not defined adm-viewer */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

