Imports System

Module Program
    REM ********************************************************************************
    REM * Prime Number P(n)                                                            *
    REM *   Programmed by G.Inuduka                                                    *
    REM ********************************************************************************

    Sub Main(args As String())
        Dim SieveSize As UInt32 = 2 ^ 30 - 1
        Dim Ans As UInt64

        Alg1a.P(1000000000, SieveSize, Ans) 'SieveSize=2^30-1:1<=n<=12283531
        'Alg2.P(162)  ' 1<=n<=162  n>162 <--- Overflow
        'Alg1.P(146)  ' 1<=n<=146  n>146 <--- Overflow
    End Sub
End Module


Module Alg2
    REM ********************************************************************************
    REM * Note : Sieve size                                                            *
    REM *               1 : Nothing                                                    *
    REM *               1 : Sieve 2                                                    *
    REM *               2 : Sieve 2 & 3                                                *
    REM *               8 : Sieve 2 & 3 & 5                                            *
    REM *              48 : Sieve 2 & 3 & 5 & 7                                        *
    REM *             480 : Sieve 2 & 3 & 5 & 7 & 11                                   *
    REM *           5,760 : Sieve 2 & 3 & 5 & 7 & 11 & 13                              *
    REM *          92,160 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17                         *
    REM *       1,658,880 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19                    *
    REM *      36,495,360 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19 & 23               *
    REM *   1,021,870,080 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19 & 23 & 29          *
    REM *   And more...                                                                *
    REM ********************************************************************************
    Function P(ByRef N As UInt16) As UInt16
        P = 0
        If N < 1 Then Exit Function

        Dim FP() As UInt64
        Dim FPP As UInt64 = 0

        Dim PC As UInt64 = 1
        Dim PP As UInt64
        Dim uCP As UInt32 = 0
        Dim P2 As UInt64 = 0

        Dim uSN() As UInt16 = {1}
        Dim uSPL As UInt64 = 1
        Dim uSL As UInt64 = 1
        Dim uSRL As UInt32
        Dim uSP As UInt32 = 0
        Dim uSCP As UInt32
        Dim uSCNP As UInt32

        Dim uADN As UInt16
        Dim uADCN As UInt16

        Dim I As UInt64 = 1
        Dim J As UInt64
        Dim K As UInt64
        Dim L As UInt64

        Dim uTMP As UInt16

        Dim bCP As Boolean = True

        Do While I <= N
            uADN = uSN(uSP)

            If P2 = PC + uADN Then
                uSL *= PP
                ReDim Preserve uSN(uSPL * PP - 1)
                L = uSPL
                For J = 2 To PP
                    For K = 0 To uSPL - 1
                        uSN(L) = uSN(K)
                        L += 1
                    Next
                Next
                uSPL *= PP
                uSRL = uSPL
                uADCN = uADN
                uSCP = uSP
                J = 0
                K = 0
                Do
                    uSCNP = uSCP + 1
                    If uSPL <= uSCNP Then
                        uSCNP -= uSPL
                    End If
                    Do While K < J
                        K += (PP + PP)
                    Loop
                    If K = J Then
                        uTMP = uSN(uSCNP)
                        uSN(uSCP) += uTMP
                        uSN(uSCNP) = 0
                        uSCNP += 1
                        J += uTMP
                    End If
                    uSCP = uSCNP
                    If uSPL <= uSCP Then
                        uSCP -= uSPL
                    End If
                    uADCN = uSN(uSCP)
                    J += uADCN
                Loop While J < uSL

                J = 0
                K = 0
                Do While K < uSPL
                    If uSN(K) = 0 Then
                        K += 1
                        uSRL -= 1
                        If K < uSP Then
                            uSP -= 1
                        End If
                    End If

                    If J <> K AndAlso K < uSPL Then
                        uSN(J) = uSN(K)
                    End If
                    J += 1
                    K += 1
                Loop

                uSPL = uSRL
                ReDim Preserve uSN(uSPL - 1)

                bCP = True
            End If

            uADN = uSN(uSP)
            PC += uADN
            uSP += 1
            If uSPL <= uSP Then
                uSP -= uSPL
            End If

            ReDim Preserve FP(FPP)
            FP(FPP) = PC
            FPP += 1

            If bCP Then
                PP = FP(uCP)
                uCP += 1
                P2 = PP * PP
                bCP = False
            End If

            Console.WriteLine("P(" & I.ToString & ")=" & PC.ToString)
            I += 1
        Loop
        P = PC
    End Function
End Module

Module Alg1a
    REM ********************************************************************************
    REM * Note : Sieve size                                                            *
    REM *               1 : Nothing                                                    *
    REM *               2 : Sieve 2                                                    *
    REM *               6 : Sieve 2 & 3                                                *
    REM *              30 : Sieve 2 & 3 & 5                                            *
    REM *             210 : Sieve 2 & 3 & 5 & 7                                        *
    REM *           2,310 : Sieve 2 & 3 & 5 & 7 & 11                                   *
    REM *          30,030 : Sieve 2 & 3 & 5 & 7 & 11 & 13                              *
    REM *         510,510 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17                         *
    REM *       9,699,690 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19                    *
    REM *     223,092,870 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19 & 23               *
    REM *   6,469,693,230 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19 & 23 & 29          *
    REM *   And more...                                                                *
    REM ********************************************************************************
    Function P(ByRef N As UInt64, ByVal SieveSize As UInt32, ByRef Ans As UInt64) As Boolean
        Ans = 0
        P = False
        If N < 1 Then Exit Function

        Dim FPP As UInt64 = 0

        Dim PC As UInt64 = 1
        Dim PP As UInt64
        Dim uCP As UInt64 = 0
        Dim P2 As UInt64 = 0

        Dim uSPL As UInt32 = 1
        Dim uSPLck As UInt64
        Dim uSP As UInt64 = 0
        Dim uSCP As UInt64
        Dim uSCNP As UInt64
        Dim uSCPE As UInt64

        Dim uADN As UInt16
        Dim uADCN As UInt16

        Dim I As UInt64 = 1
        Dim J As UInt64
        Dim K As UInt64
        Dim L As UInt64

        Dim uTMP As UInt16

        Dim fSOV As Boolean = False
        Dim bCP As Boolean = True

        Dim uSN(SieveSize) As UInt32
        uSN(0) = 1
        Dim FP(SieveSize) As UInt64

        Do While I <= N
            uADN = uSN(uSP)

            If P2 = PC + uADN Then
                Console.WriteLine("Rebuild Sieve..." & vbTab & I & vbTab & P2)
                If Not fSOV Then
                    uSPLck = uSPL * PP - 1
                    If SieveSize < uSPLck Then
                        fSOV = True
                        Console.WriteLine("Sieve Clip")
                    End If
                End If

                If Not fSOV Then
                    Do
                        L = uSPL
                        For J = 2 To PP
                            For K = 0 To uSPL - 1
                                uSN(L) = uSN(K)
                                L += 1
                                If SieveSize < L Then
                                    uSPL = SieveSize
                                    Exit Do
                                End If
                            Next
                        Next
                        uSPL *= PP
                    Loop While False
                    uSCPE = uSPL
                Else
                    uSCPE = uSPL - uSP
                End If

                uSCP = uSP
                uADCN = uADN
                J = 0
                K = 0
                Do
                    Do While K < J
                        K += (PP + PP)
                    Loop
                    uSCNP = uSCP + uADCN
                    If uSPL <= uSCNP Then
                        uSCNP -= uSPL
                    End If
                    If K = J Then
                        uTMP = uSN(uSCNP)
                        uSN(uSCP) += uTMP
                        uSN(uSCNP) = 0
                        uSCNP += uTMP
                        J += uTMP
                    End If
                    uSCP = uSCNP
                    If uSPL <= uSCP Then
                        uSCP -= uSPL
                    End If
                    uADCN = uSN(uSCP)
                    J += uADCN
                Loop While J < uSCPE
                bCP = True
            End If

            uADN = uSN(uSP)
            PC += uADN
            uSP += uADN
            If uSPL <= uSP Then
                If fSOV Then
                    P = True
                    Exit Do
                End If
                uSP -= uSPL
            End If

            FP(FPP) = PC
            FPP += 1

            If bCP Then
                PP = FP(uCP)
                uCP += 1
                P2 = PP * PP
                bCP = False
            End If

            Console.WriteLine("P(" & I & ")=" & PC)
            I += 1
        Loop
        If P Then
            Console.WriteLine("Sieve is end.")
        End If
        Ans = PC
    End Function
End Module


Module Alg1
    REM ********************************************************************************
    REM * Note : Sieve size                                                            *
    REM *               1 : Nothing                                                    *
    REM *               2 : Sieve 2                                                    *
    REM *               6 : Sieve 2 & 3                                                *
    REM *              30 : Sieve 2 & 3 & 5                                            *
    REM *             210 : Sieve 2 & 3 & 5 & 7                                        *
    REM *           2,310 : Sieve 2 & 3 & 5 & 7 & 11                                   *
    REM *          30,030 : Sieve 2 & 3 & 5 & 7 & 11 & 13                              *
    REM *         510,510 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17                         *
    REM *       9,699,690 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19                    *
    REM *     223,092,870 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19 & 23               *
    REM *   6,469,693,230 : Sieve 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19 & 23 & 29          *
    REM *   And more...                                                                *
    REM ********************************************************************************
    Function P(ByRef N As UInt16) As UInt16
        P = 0
        If N < 1 Then Exit Function

        Dim FP() As UInt64
        Dim FPP As UInt64 = 0

        Dim PC As UInt64 = 1
        Dim PP As UInt64
        Dim uCP As UInt32 = 0
        Dim P2 As UInt64 = 0

        Dim uSN() As UInt16 = {1}
        Dim uSPL As UInt32 = 1
        Dim uSP As UInt32 = 0
        Dim uSCP As UInt32
        Dim uSCNP As UInt32

        Dim uADN As UInt16
        Dim uADCN As UInt16

        Dim I As UInt32 = 1
        Dim J As UInt32
        Dim K As UInt32
        Dim L As UInt32

        Dim uTMP As UInt16

        Dim bCP As Boolean = True

        Do While I <= N
            uADN = uSN(uSP)

            If P2 = PC + uADN Then
                ReDim Preserve uSN(uSPL * PP - 1)
                L = uSPL
                For J = 2 To PP
                    For K = 0 To uSPL - 1
                        uSN(L) = uSN(K)
                        L += 1
                    Next
                Next
                uSPL *= PP

                uSCP = uSP
                uADCN = uADN
                J = 0
                K = 0
                Do
                    Do While K < J
                        K += (PP + PP)
                    Loop
                    uSCNP = uSCP + uADCN
                    If uSPL <= uSCNP Then
                        uSCNP -= uSPL
                    End If
                    If K = J Then
                        uTMP = uSN(uSCNP)
                        uSN(uSCP) += uTMP
                        uSN(uSCNP) = 0
                        uSCNP += uTMP
                        J += uTMP
                    End If
                    uSCP = uSCNP
                    If uSPL <= uSCP Then
                        uSCP -= uSPL
                    End If
                    uADCN = uSN(uSCP)
                    J += uADCN
                Loop While J < uSPL
                bCP = True
            End If

            uADN = uSN(uSP)
            PC += uADN
            uSP += uADN
            If uSPL <= uSP Then
                uSP -= uSPL
            End If

            ReDim Preserve FP(FPP)
            FP(FPP) = PC
            FPP += 1

            If bCP Then
                PP = FP(uCP)
                uCP += 1
                P2 = PP * PP
                bCP = False
            End If

            Console.WriteLine("P(" & I.ToString & ")=" & PC.ToString)
            I += 1
        Loop
        P = PC
    End Function
End Module
