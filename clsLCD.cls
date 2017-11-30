Public Class cls_LCD

    Public v_numero As String = ""
    Public numeroImpv_Lista() As String
    Public v_EjeX As String = "X"
    Public v_EjeY As String = "Y"
    Public v_Signo_Vertical As String = "|"
    Public v_Signo_Horizontal As String = "-"
    Public SIZE As Integer = 0
    Public arrNumeros() As String 'obtengo los numero
    Public Tfilas As Integer = 0
    Public col As Integer = 0
    Public tcol As Integer = 0
    Public matriz(,) As String 'creo la matriz
    Public v_salto As Integer
    Public v_sarta As String = ""


    Public pf1(2) As Integer
    Public pf2(2) As Integer
    Public pf3(2) As Integer
    Public pf4(2) As Integer
    Public pf5(2) As Integer
#Region "PROCEDIMIENTOS DEL FORMULARIO"
    Public Function fu_Finaliza(ByVal In_Size As Integer, In_Valores As Integer) As Boolean
        'PROCEDIMIENTO QUE VALIDA EL VALOR (0,0) DE FINALIZACION DE LA APLICACION
        Dim v_Rta As Boolean = False
        If In_Size = "0" AndAlso In_Valores = "0" Then
            v_Rta = True
        End If
        Return v_Rta
    End Function
    Public Function fu_TamanoPermitido(ByVal In_Size As Integer) As Boolean
        'PROCEDIMIENTO QUE VALIDA EL VALOR DEL TAMAÑO PERMITIDO PARA LA DIGITALIZACION DEL NUMERO
        Dim v_Rta As Boolean = False
        If In_Size < 1 OrElse In_Size > 10 Then
            v_Rta = True
        End If
        Return v_Rta
    End Function
    Public Sub lo_procesarNumero(ByVal In_Numero As String)
        'SE REALIZA UNA PARTICION DEL LA REPRESENTACION DEL NUMERO EN SEGMENTOS POR EL SIGNO COMA (",")
        'SE RECORRE EL NUEVO ARRAY Y SE ACTIVA EL SEGMENTO DE ACUERDO A SU UBICACION, TAMAÑO, SIGO O CARACTERES Y LOS VALORES DE LOS SALTOS Y UBICACION
        'For i = 0 To arrNumeros.Length - 1
        Dim v_Numeros() As String = In_Numero.Split(",")
        For y = 0 To v_Numeros.Length - 1
            Select Case v_Numeros(y)
                Case 1 'Horizontal
                    lo_LlenarMatriz(SIZE, v_EjeY, pf1, v_Signo_Vertical)
                Case 2 'Horizontal
                    lo_LlenarMatriz(SIZE, v_EjeY, pf2, v_Signo_Vertical)
                Case 3 'Horizontal
                    lo_LlenarMatriz(SIZE, v_EjeY, pf5, v_Signo_Vertical)
                Case 4 'Horizontal
                    lo_LlenarMatriz(SIZE, v_EjeY, pf4, v_Signo_Vertical)
                Case 5 'Vertical
                    lo_LlenarMatriz(SIZE, v_EjeX, pf1, v_Signo_Horizontal)
                Case 6 'Vertical
                    lo_LlenarMatriz(SIZE, v_EjeX, pf2, v_Signo_Horizontal)
                Case 7 'Vertical
                    lo_LlenarMatriz(SIZE, v_EjeX, pf3, v_Signo_Horizontal)
            End Select
        Next
        'Next
    End Sub
    Public Sub lo_LlenarMatriz(ByVal In_Size As Integer, ByVal In_Eje As String, ByVal in_posicion As Integer(), ByVal In_Signo As String)
        ' SE MAPEA LA UBICACION EN LA MATRIZ DONDE VA ALMACENAR EL VALOR DEL SIGNO O CARACTER
        If v_EjeX = In_Eje Then
            For y As Integer = 1 To In_Size
                Dim valor As Integer = in_posicion(1) + y
                matriz(in_posicion(0), valor) = In_Signo
            Next
        ElseIf v_EjeY = In_Eje Then
            For i As Integer = 1 To In_Size
                Dim valor As Integer = in_posicion(0) + i
                matriz(valor, in_posicion(1)) = In_Signo
            Next
        End If
    End Sub
    Public Function fu_Numero(ByVal In_Numero As Integer) As String
        'FUNCION QUE DEVUELVE EL VALOR DEL NUMERO POR SEGMENTOS.
        'EL VALOR DE LOS SEGMETOS DE DIGITALIZACION SE COMPONE DE LA SIGUIENTE FORMA
        'SEG = SEGMENTO
        '                   SEG 5
        '                  --------
        '                 |        |
        '                 |        |
        '           SEG 1 |        | SEG 3
        '                 |        |
        '                 | SEG 6  |
        '                  -------- 
        '                 |        |
        '                 |        |
        '           SEG 2 |        | SEG 4
        '                 |        |
        '                 | SEG 7  |
        '                  -------- 

        Dim v_Rta As String = String.Empty
        Select Case In_Numero
            Case 0
                v_Rta = "1,2,3,4,5,7"
            Case 1
                v_Rta = "3,4"
            Case 2
                v_Rta = "5,3,6,2,7"
            Case 3
                v_Rta = "5,3,6,4,7"
            Case 4
                v_Rta = "1,6,3,4"
            Case 5
                v_Rta = "5,1,6,4,7"
            Case 6
                v_Rta = "5,1,6,2,4,7"
            Case 7
                v_Rta = "5,3,4"
            Case 8
                v_Rta = "1,2,3,4,5,6,7"
            Case 9
                v_Rta = "1,3,4,5,6,7"
        End Select
        Return v_Rta
    End Function
    Public Function lo_ComienzaProceso(ByVal numeroImp() As String, ByVal espacio As Integer) As String
        v_salto = 0
        SIZE = numeroImp(0)
        Tfilas = SIZE * 2 + 3 'Calculo del total de filas
        col = SIZE + 2 'columnas
        tcol = (1 * numeroImp(1).Length) + (col * numeroImp(1).Length) 'total de Columnas
        ReDim matriz(Tfilas, tcol) 'Se Redimiensiona la matriz
        For i = 0 To Tfilas - 1 'se Llena la matriz con un valor por defecto
            For y = 0 To tcol - 1
                matriz(i, y) = " "
            Next
        Next

        'COMIENZA LA DIGITALIZACION DEL SEGUNDO GRUPO DE NUMEROS
        For i = 0 To numeroImp(1).Length - 1

            pf1(0) = 0
            pf1(1) = 0 + v_salto

            pf2(0) = Math.Floor((Tfilas / 2)) 'AQUI HABIA OTRO ERROR PORQUE AGREGABA UNA FILA AL DEJAR UN NUMERO APROXIMADO AL SIGUIENTE DIGITO
            pf2(1) = 0 + v_salto

            pf3(0) = (Tfilas - 1)
            pf3(1) = 0 + v_salto

            pf4(0) = (col - 1)
            pf4(1) = Math.Floor((Tfilas / 2)) + v_salto 'AQUI HABIA OTRO ERROR PORQUE AGREGABA UNA FILA AL DEJAR UN NUMERO APROXIMADO AL SIGUIENTE DIGITO

            pf5(0) = 0
            pf5(1) = (col - 1) + v_salto

            v_salto = v_salto + col + 1
            'SE OBTIENE LA REPRESENTACION DEL NUMERO EN SEGMENTOS LCD - A TRAVES DE LA FUNCION OBTENER NUMERO
            lo_procesarNumero(fu_Numero(CInt(numeroImp(1).Substring(i, 1))))
        Next

        'SE REALIZA LA IMPRESION DE LA MATRIZ CON LOS NUMEROS DIGITALIZADOS
        lo_ImprimirMatriz(Tfilas, tcol)
        Return v_sarta
    End Function
    Public Sub lo_ImprimirMatriz(ByVal In_Filas As Integer, ByVal In_Columnas As Integer)
        'SE RECORRE LA MATRIZ POR FILAS * COLUMNAS 
        'SE EXTRAE EL VALOR DE LA POSICION (I, Y) 
        'SE CONCATENA EL VALOR DE LA MATRIZ EN UNA VARIABLE 
        For i = 0 To In_Filas - 1
            For y = 0 To In_Columnas - 1
                v_sarta &= matriz(i, y)
            Next
            'lg_bitacora(v_sarta, RichTextBox1) 'METODO O FUNCION PARA REALIZAR UNA IMPRESION EN UN CAJA DE TEXTO
            v_sarta &= Chr(13)
        Next
    End Sub

#End Region

End Class
