/*
 * Prototype include file: src/web/escrprto.i
 * Created from procedure: src/web/escript.p
 */

FUNCTION appendBytes RETURNS LOGICAL
  (INPUT RAW) IN SUPER.

FUNCTION appendString RETURNS LOGICAL
  (INPUT CHARACTER) IN SUPER.

FUNCTION appendMemptr RETURNS LOGICAL
  (INPUT MEMPTR) IN SUPER.

FUNCTION disablePageBuffer RETURNS LOGICAL IN SUPER.

FUNCTION disablePageCache RETURNS LOGICAL IN SUPER.

FUNCTION errorPage RETURNS LOGICAL
  (INPUT pcError AS CHARACTER) IN SUPER.

FUNCTION evaluateScript RETURNS LOGICAL
  (INPUT pcHTML AS CHARACTER,
   OUTPUT pcErrorMsg AS CHARACTER) IN SUPER.

FUNCTION expandScript RETURNS CHARACTER
  (INPUT pcHTML AS CHARACTER) IN SUPER.

FUNCTION fetchMarkup RETURNS CHARACTER
  (INPUT pcMarkupFile AS CHARACTER) IN SUPER.

FUNCTION flushStream RETURNS LOGICAL IN SUPER.

FUNCTION getContentType RETURNS CHARACTER IN SUPER.

FUNCTION getMessages RETURNS CHARACTER
  (INPUT pcGroup AS CHARACTER,
   INPUT plDelete AS LOGICAL) IN SUPER.

FUNCTION hasMessages RETURNS LOGICAL
  (INPUT pcGroup AS CHARACTER) IN SUPER.

FUNCTION initPage RETURNS LOGICAL IN SUPER.

FUNCTION isDefined RETURNS LOGICAL
  (INPUT CHARACTER) IN SUPER.

FUNCTION isGet RETURNS LOGICAL IN SUPER.

FUNCTION isPost RETURNS LOGICAL IN SUPER.

FUNCTION isSelected RETURNS CHARACTER
  (INPUT pcField AS CHARACTER,
   INPUT pcValue AS CHARACTER,
   INPUT pcSelected AS CHARACTER) IN SUPER.

FUNCTION outputHeader RETURNS LOGICAL IN SUPER.

FUNCTION outputPage RETURNS LOGICAL IN SUPER.

FUNCTION processMarkup RETURNS LOGICAL
  (INPUT pcMarkupName AS CHARACTER) IN SUPER.

FUNCTION queueMessage RETURNS LOGICAL
  (INPUT pcGroup AS CHARACTER,
   INPUT pcMessage AS CHARACTER) IN SUPER.

FUNCTION setCookies RETURNS LOGICAL IN SUPER.

FUNCTION setPreprocess RETURNS LOGICAL
  (INPUT plPreprocess AS LOGICAL) IN SUPER.

FUNCTION showField RETURNS LOGICAL
  (INPUT pcFieldName AS CHARACTER,
   INPUT pcHTML AS CHARACTER,
   OUTPUT pcError AS CHARACTER) IN SUPER.

FUNCTION streamFile RETURNS LOGICAL
  (INPUT pcFileName AS CHARACTER) IN SUPER.

FUNCTION writeFile RETURNS LOGICAL
  (INPUT pcFilename AS CHARACTER) IN SUPER.

FUNCTION getPageBuffer RETURNS MEMPTR IN SUPER.