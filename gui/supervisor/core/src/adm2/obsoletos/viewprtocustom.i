/*
 * Prototype include file: C:\custom9new\src\adm2\viewprtocustom.i
 * Created from procedure: C:\custom9new\src\adm2\viewercustom.p at 12:56 on 19/02/01
 * by the PROGRESS PRO*Tools Prototype Include File Generator
 */

PROCEDURE createText IN SUPER:
  DEFINE INPUT PARAMETER hField AS HANDLE.
  DEFINE OUTPUT PARAMETER hText AS HANDLE.
END PROCEDURE.

PROCEDURE displayDescriptors IN SUPER:
  DEFINE INPUT PARAMETER xFieldHandles AS CHARACTER.
END PROCEDURE.

PROCEDURE displayFields IN SUPER:
  DEFINE INPUT PARAMETER pcColValues AS CHARACTER.
END PROCEDURE.

PROCEDURE displayFieldsWithProblem IN SUPER:
  DEFINE INPUT PARAMETER cFields AS CHARACTER.
END PROCEDURE.

PROCEDURE selectDescriptor IN SUPER:
  DEFINE INPUT PARAMETER hField AS HANDLE.
  DEFINE INPUT PARAMETER hText AS HANDLE.
  DEFINE INPUT PARAMETER xTable AS CHARACTER.
  DEFINE INPUT PARAMETER xColumn AS CHARACTER.
  DEFINE INPUT PARAMETER xWhere AS CHARACTER.
  DEFINE INPUT PARAMETER xCompatibleFieldName AS CHARACTER.
END PROCEDURE.

PROCEDURE selectField IN SUPER:
  DEFINE INPUT PARAMETER hField AS HANDLE.
END PROCEDURE.

PROCEDURE setRelatedFields IN SUPER:
  DEFINE INPUT PARAMETER hProcedure AS HANDLE.
END PROCEDURE.

FUNCTION getObjectType RETURNS CHARACTER IN SUPER.

