      Program prgm_01_02
!
!     This program reads two 3x3 matrices from user-provided input files. After the
!     files are opened and read, they are closed and the matrices printed.
!
!     H. P. Hratchian, 2019.
!
      implicit none
      integer,parameter::inFileUnitA=10,inFileUnitB=11
      integer::errorFlag,i
      real,dimension(3,3)::matrixInA,matrixInB
      character(len=128)::fileNameA,fileNameB
!
!
!     Start by asking the user for the name of the data file.
!
      write(*,*)' What are the names of the input data files?'
      read(*,*) fileNameA,fileNameB
!
!     Open the data file and read matrixInA from that file.
!
      open(unit=inFileUnitA,file=TRIM(fileNameA),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitA,*) matrixInA(1,i),matrixInA(2,i),matrixInA(3,i)
      endDo
      close(inFileUnitA)
!     Open the data file and read matrixInB from that file. 
      open(unit=inFileUnitB,file=TRIM(fileNameB),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitB,*) matrixInB(1,i),matrixInB(2,i),matrixInB(3,i)
      endDo
      close(inFileUnitB)

!     Call the subroutine PrintMatrix to print matrixInA.
!
      call PrintMatrix3x3(matrixInA)
!     Call the subroutine PrintMatrix to pring matrixInB.
      call PrintMatrix3x3(matrixInB) 
!
  999 continue
      End Program prgm_01_02


      Subroutine PrintMatrix3x3(matrix)
!
!     This subroutine prints a 3x3 real matrix. The output is written to StdOut.
!
      implicit none
      real,dimension(3,3),intent(in)::matrix
      integer::i
!
!     Format statements.
!
 1000 format(3(2x,f5.1))
!
!     Do the printing job.
!
      write(*,*)' Printing Matrix'
!
! ADD CODE HERE
!
!
      DO i = 1,3

      write(*,1000) matrix(i,1),matrix(i,2),matrix(i,3)
      endDo
      return
      End Subroutine PrintMatrix3x3
