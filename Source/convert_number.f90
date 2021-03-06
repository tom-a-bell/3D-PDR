!=======================================================================
!
!  Functions to convert numeric values to their appropriate string
!  representation, based on the type and precision of the number.
!  The function is overloaded, so can be called regardless of the
!  type (integer or real) of the input variable.
!
!-----------------------------------------------------------------------
MODULE NUM2STR_FUNCTION

   INTERFACE NUM2STR
      MODULE PROCEDURE CONVERT_INTEGER,CONVERT_REAL,CONVERT_PRECISION
   END INTERFACE NUM2STR

CONTAINS

   FUNCTION CONVERT_INTEGER(NUMBER) RESULT(STRING)
      USE HEALPIX_TYPES
      IMPLICIT NONE
      INTEGER(KIND=I4B), INTENT(IN) :: NUMBER
      CHARACTER(LEN=20) :: STRING
      WRITE(STRING,*) NUMBER
      STRING = TRIM(ADJUSTL(STRING))
      RETURN
   END FUNCTION CONVERT_INTEGER

   FUNCTION CONVERT_REAL(NUMBER) RESULT(STRING)
      USE HEALPIX_TYPES
      IMPLICIT NONE
      REAL(KIND=DP), INTENT(IN) :: NUMBER
      CHARACTER(LEN=20) :: STRING
      IF(ABS(NUMBER).GE.1.0D6) THEN
         WRITE(STRING,"(ES13.5)") NUMBER
      ELSE IF(ABS(NUMBER).LT.1.0D-5) THEN
         WRITE(STRING,"(ES13.5)") NUMBER
      ELSE IF(ABS(NUMBER).GE.1.0D5) THEN
         WRITE(STRING,"(F10.1)") NUMBER
      ELSE IF(ABS(NUMBER).GE.1.0D4) THEN
         WRITE(STRING,"(F10.2)") NUMBER
      ELSE IF(ABS(NUMBER).GE.1.0D3) THEN
         WRITE(STRING,"(F10.3)") NUMBER
      ELSE IF(ABS(NUMBER).GE.1.0D2) THEN
         WRITE(STRING,"(F10.4)") NUMBER
      ELSE IF(ABS(NUMBER).GE.1.0D0) THEN
         WRITE(STRING,"(F10.5)") NUMBER
      ELSE
         WRITE(STRING,"(F10.6)") NUMBER
      END IF
      STRING = TRIM(ADJUSTL(STRING))
      RETURN
   END FUNCTION CONVERT_REAL


   FUNCTION CONVERT_PRECISION(NUMBER,PRECISION) RESULT(STRING)
      USE HEALPIX_TYPES
      IMPLICIT NONE
      REAL(KIND=DP),     INTENT(IN) :: NUMBER
      INTEGER(KIND=I4B), INTENT(IN) :: PRECISION
      CHARACTER(LEN=50) :: STRING,FORMAT
      IF(ABS(NUMBER).GE.1.0D4**PRECISION) THEN
         WRITE(FORMAT,*) PRECISION
         FORMAT = "(ES50."//TRIM(ADJUSTL(FORMAT))//"E1)"
         WRITE(STRING,FORMAT) NUMBER
      ELSE IF(ABS(NUMBER).LT.1.0D1**-PRECISION) THEN
         WRITE(FORMAT,*) PRECISION
         FORMAT = "(ES50."//TRIM(ADJUSTL(FORMAT))//"E1)"
         WRITE(STRING,FORMAT) NUMBER
      ELSE
         WRITE(FORMAT,*) PRECISION
         FORMAT = "(F50."//TRIM(ADJUSTL(FORMAT))//")"
         WRITE(STRING,FORMAT) NUMBER
      END IF
      STRING = TRIM(ADJUSTL(STRING))
      RETURN
   END FUNCTION CONVERT_PRECISION

END MODULE NUM2STR_FUNCTION
!=======================================================================
