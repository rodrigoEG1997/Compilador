import re
import sys

#Variables globales de maquina virtual
contMemoriaMV = 0 #apuntador a la memoria que esta (en caso de tener memorias temporales)
mem = str(contMemoriaMV) #memoria en la que esta en string para manipular mejor
memoriaMV = {mem:[]} #se inicializa la memoria
mFun = []#lista de memoria de variables y sus espacios de memoria en la memoria temporal
listaRegresa = [] #Guarda el apuntador de los cuaduplos para poder regresa cuando se termine la funcion
valorRegresa = [] #Guarda los valores de los returns de las funciones
regresa = False #saber si asignar el valor de regresa
funcActual = None #tener la funcion actual
listaMemFun = [] #lista de mFun, para guardar las memorias temporales (en caso de recursividad)

#Crea la memoria de la maquina virtual
for i in range(14000):
    memoriaMV[mem].append(None)


#Funcion principal de la maquina virtual (es la que el compilador manda llamar para ejecutar los cuadruplos)
def virtual(cuadruplos,varConst,memoriaFunciones,listaApuntador):
  
    #llamada a variables globales
    global contMemoriaMV
    global mem
    global valorRegresa
    global regresa
    global funcActual
    global mFun
    global listaMemFun
    
    #Se asigna en la memoria de la maquina virtual todas las constantes encontradas por el compilador
    for i in varConst:
        memoriaMV[mem][varConst[i]['memoria']] = varConst[i]['nombre']
    
    #Apuntador empieza en el cuadruplo 0
    apuntador = 0
    #instr es para leer los cuadruplos
    instr = cuadruplos[0].split()
    
    #El programa se va a ejecutar hasta que encuentre el cuadruplo endProgram
    while instr[0] != 'endProgram':
        #se lee el cuadruplo actual
        instr = cuadruplos[apuntador].split()
        
        #En caso de goto
        if instr[0] == 'Goto':
            #cambia el apuntador a lo que el goto dice
            apuntador = int(instr[1])
        #En caso de asignacion
        elif instr[0] == '=':
            #busca el cuadruplo anterior (Para poner el parche de regreso de funcion)
            gosub = cuadruplos[apuntador - 1].split()
            #Valida si el cuadruplo anterior fue goSub (Para poner el parche de regreso de funcion)
            if gosub[0] == 'GoSub' and regresa:
                #checa si estas en el principal o en una funcion (para el manejo de memoria)
                if contMemoriaMV > 0:
                    #En caso de estar en una funcion, agrega el espacio de memoria que tenia la funcion
                    addMemFun(int(instr[1]),memoriaFunciones)
                    regresa = False
                    #Se le asigna el valor de retorno 
                    memoriaMV[mem].append(None)
                    memoriaMV[mem][mandarDir(fM(int(instr[1])))] = valorRegresa.pop() #Los valores de regreso de funcion se guardan en esta lista
                else:
                    #Si esta en la funcion principal, solamente se le asigna al espacio de memoria lo que regreso la funcion
                    memoriaMV[mem][mandarDir(fM(int(instr[1])))] = valorRegresa.pop() #Los valores de regreso de funcion se guardan en esta lista
                    regresa = False
            #Se realiza la asignacion a las variables
            memoriaMV[mem][mandarDir(fM(int(instr[2])))] = memoriaMV[mem][mandarDir(fM(int(instr[1])))]
            apuntador += 1
        elif instr[0] == '+':
            #Se encuentra las direcciones de memoria de los numeros 
            num1 = memoriaMV[mem][mandarDir(fM(int(instr[1])))]
            num2 = memoriaMV[mem][mandarDir(fM(int(instr[2])))]
            #Valida si se puede hacer la operacion
            validarOp(num1,num2,apuntador)
            #checa si el cuadruplo es mayor a dos (en caso de que sea array se hacen dos sumas obligatorias)
            if apuntador > 2:
                #guarda el cuadruplo de dos direcciones anteriores
                ver = cuadruplos[apuntador - 2].split()
                num1 = memoriaMV[mem][mandarDir(fM(int(instr[1])))]
                num2 = memoriaMV[mem][mandarDir(fM(int(instr[2])))]
                #manda a llamar la funcion que hace las validaciones de los arreglos
                addDireccion(ver, num1, num2,memoriaFunciones,funcActual)
            #Se realiza la operacion con las direcciones de memoria asignadas
            memoriaMV[mem][fM(int(instr[3]))] = memoriaMV[mem][mandarDir(fM(int(instr[1])))] + memoriaMV[mem][mandarDir(fM(int(instr[2])))]
            apuntador += 1
        elif instr[0] == '-':
            #Se encuentra las direcciones de memoria de los numeros 
            num1 = memoriaMV[mem][mandarDir(fM(int(instr[1])))]
            num2 = memoriaMV[mem][mandarDir(fM(int(instr[2])))]
            #Valida si se puede hacer la operacion
            validarOp(num1,num2,apuntador)
            #Se realiza la operacion con las direcciones de memoria asignadas
            memoriaMV[mem][fM(int(instr[3]))] = memoriaMV[mem][mandarDir(fM(int(instr[1])))] - memoriaMV[mem][mandarDir(fM(int(instr[2])))]
            apuntador += 1
        elif instr[0] == '*':
            #Se encuentra las direcciones de memoria de los numeros
            num1 = memoriaMV[mem][mandarDir(fM(int(instr[1])))]
            num2 = memoriaMV[mem][mandarDir(fM(int(instr[2])))]
            #Valida si se puede hacer la operacion
            validarOp(num1,num2,apuntador)
            #Se realiza la operacion con las direcciones de memoria asignadas
            memoriaMV[mem][fM(int(instr[3]))] = memoriaMV[mem][mandarDir(fM(int(instr[1])))] * memoriaMV[mem][mandarDir(fM(int(instr[2])))]
            apuntador += 1
        elif instr[0] == '/':
            #Se encuentra las direcciones de memoria de los numeros
            num1 = memoriaMV[mem][mandarDir(fM(int(instr[1])))]
            num2 = memoriaMV[mem][mandarDir(fM(int(instr[2])))]
            #Valida si se puede hacer la operacion
            validarOp(num1,num2,apuntador)
            #Se realiza la operacion con las direcciones de memoria asignadas
            memoriaMV[mem][fM(int(instr[3]))] = memoriaMV[mem][mandarDir(fM(int(instr[1])))] / memoriaMV[mem][mandarDir(fM(int(instr[2])))]
            apuntador += 1
        elif instr[0] == '<' or instr[0] == '<=' or instr[0] == '>' or instr[0] == '>=' or instr[0] == '==' or instr[0] == '&' or instr[0] == '|':
            #Se realiza la operacion con las direcciones de memoria asignadas
            #funcion compararVar regresa si es True o False la exprecion para guardarlo en la memoria asignada
            memoriaMV[mem][fM(int(instr[3]))] = compararVar(instr[0],memoriaMV[mem][mandarDir(fM(int(instr[1])))],memoriaMV[mem][mandarDir(fM(int(instr[2])))])
            apuntador += 1
        #En caso de GotoFalse
        elif instr[0] == 'GotoF':
            #checa si es True
            if memoriaMV[mem][fM(int(instr[1]))]:
                #Si es true lo ignora
                apuntador += 1
            else:
                #Cambia el apuntador a donde dice el gotoF
                apuntador = int(instr[2])
        #En caso de escribir
        elif instr[0] == 'Escr':
            #Escribe el nombre que esta en la direccion de memoria (valida si es un apuntador)
            print(memoriaMV[mem][mandarDir(fM(int(instr[1])))])
            apuntador += 1
        #En caso de lee
        elif instr[0] == 'Lee':
            #Lee la variable el valor de la variable y lo guarda en la direccion de memoria
            #validarInput valida que el tipo de ingreso sea compativle con la variable a la que se le va a asignar el valor
            memoriaMV[mem][mandarDir(fM(int(instr[1])))] = validarInput(input(),int(instr[1]))
            apuntador += 1
        #En caso de arreglo
        elif instr[0] == 'Ver':
            #Verifica si la variable se encuentra entre los limites del arreglo
            if memoriaMV[mem][mandarDir(fM(int(instr[1])))] < memoriaMV[mem][fM(int(instr[2]))] or memoriaMV[mem][mandarDir(fM(int(instr[1])))] >= memoriaMV[mem][fM(int(instr[3]))]:
                print("ERROR, parametro de arreglo supera las dimenciones")
                sys.exit(1)
            apuntador += 1
        #Cuando se detecta una funcion
        elif instr[0] == 'ERA':
            #Se guarda el nombre de la funcion
            funcActual = instr[1]
            #Se manda a crear el espacio de memoria de la nueva funcion
            era(memoriaFunciones,instr[1],varConst,listaApuntador)
            apuntador += 1
        #Para los parametros
        elif instr[0] == 'Param':
            #Agrega en la memoria de la funcion los valores del los parametros que se asignaron
            addParam(int(instr[1]),varConst)
            #Se guarda l amemoria del parametro
            memoriaParam = memoriaMV[mem][mandarDir(fM(int(instr[1])))]
            contMemoriaMV += 1
            #Valida que si es constante, se ponga automaticamente le valor, ya que todas las constantes estan en la lista de constantes
            if int(instr[1]) >= 6000 and int(instr[1]) < 9000:
                for i in varConst:
                    if varConst[i]['memoria'] == int(instr[1]):
                        memoriaMV[str(contMemoriaMV)][fM(int(instr[2]))] = varConst[i]['nombre']
            else:
                #Si no es constante, se asigna los valores de los parametros
                #mem = 1                                            mem = 0
                memoriaMV[str(contMemoriaMV)][fM(int(instr[2]))] = memoriaParam
            contMemoriaMV -= 1
            apuntador += 1
        elif instr[0] == 'GoSub':
            #Se mueve el apuntador de memoria a la memoria de la funcion actual (Se hace hasta ahorita para sacar el valor de los parametros)
            contMemoriaMV += 1
            mem = str(contMemoriaMV)
            #Guarda el cuadruplo en el que se quedo para cuando se termine la funcion
            listaRegresa.append(apuntador)
            apuntador = int(instr[1])
        #En caso de regresa
        elif instr[0] == 'regresa':
            #Guarda el valor de retorno en la lista valorRegresa para poder asignarlo en un futuro
            valorRegresa.append(memoriaMV[mem][mandarDir(fM(int(instr[1])))])
            regresa = True
            apuntador += 1
        #Se detecta que se termino la funcion
        elif instr[0] == 'endFunc':
            funcActual = None
            #Se borra la memoria que se habia asignado a la funcion
            del memoriaMV[mem]
            #Se mueve el contador de memoria a la memoria de la funcion previa
            contMemoriaMV -= 1
            mem = str(contMemoriaMV)
            #se regresa a el cuadruplo en el que estaba
            apuntador = listaRegresa.pop() + 1
            #Para recursividad, guardar las listas de memoria previas 
            if contMemoriaMV > 0:
                mFun = listaMemFun.pop()
        else:
            apuntador += 1

#Valida si la memoria esta en la lista de meoria de funcion y si no la agrega
def addMemFun(num,memoriaFunciones):
    if num not in mFun:
        mFun[num] = len(mFun)
        memoriaMV[mem].append(None)


#Aggrega la memoria a la que se referencia los arregloss
def addDireccion(ver, num1, num2,memoriaFunciones,instr):
    #Valida si es Ver
    if ver[0] == 'Ver':
        
        #Crea la memoria
        if funcActual != None:
            addMem = num1 + num2

            #Tamaño, para saber cual es el numero que sigue de la memoria
            tam = len(memoriaFunciones[instr])

            #agrega la memoria si no estaba
            if addMem not in memoriaFunciones[instr]:
                memoriaFunciones[instr][addMem] = tam
                memoriaMV[mem].append(memoriaMV['0'][addMem])

#Agrega la memoria del valor de los parametros  
def addParam(num,varConst):
    global mFun
    espacio = len(mFun)

    #Valida que no este la memoria y la agrega
    if num not in mFun:
        mFun[num] = espacio
        memoriaMV[str(contMemoriaMV + 1)].append(None)


#Crea el nuevo espacio de memoria para la funcion 
def era(memoriaFunciones, instr, varConst,listaApuntador):
    global memoriaMV
    global mFun
    global listaMemFun


    iniciaList = 0

    for i in memoriaFunciones[instr]:
        memoriaFunciones[instr][i] = iniciaList
        iniciaList += 1

    #Valida si esta en principal o no y en caso de no estarlo guarda el espacio de memoria provicional
    if contMemoriaMV > 0:
        listaMemFun.append(mFun)

    #Crea la lista que referencia a las memorias
    mFun = memoriaFunciones[instr]

        
    add = True
    #Crea la nueva lista de memorias
    memoriaMV[str(contMemoriaMV + 1)] = []

    memCero = None

    #Agrega la memoria de la constante 0
    for i in varConst:
        if varConst[i]['nombre'] == 0:
            memCero = varConst[i]['memoria']
    
    tam = len(memoriaFunciones[instr])

    if memCero not in memoriaFunciones[instr]:
        memoriaFunciones[instr][memCero] = tam

    #Crea la memoria con el valor de las constantes ya predefinidp
    for i in memoriaFunciones[instr]:
        for j in varConst:
            #varConst tiene todas las constantes que se utilizaron en el programa
            if i == varConst[j]['memoria']:
                memoriaMV[str(contMemoriaMV + 1)].append(varConst[j]['nombre']) 
                add = False
                break  
        if add:  
            memoriaMV[str(contMemoriaMV + 1)].append(None)
        add = True

    #Creo que esta mal
    if contMemoriaMV > 0:
        listaMemFun

    #Le asigna las globales a la memoria provicional de la funcion
    for j in mFun:
        if j >= 12000:
            memoriaMV[str(contMemoriaMV + 1)][mFun[j]] = memoriaMV['0'][listaApuntador[j]]

        if j < 3000:
            memoriaMV[str(contMemoriaMV + 1)][mFun[j]] = memoriaMV['0'][j]
    


#Direcciona la memoria para el verdadero espacio de memoria
#Solamente se usa en los espacios de memoria temporal
def fM(num):
    global mFun

    if contMemoriaMV > 0:
        return mFun[num]
    return num

#Asigna el valor que tenia esa variable en la memoria anterior
#Se utiliza en los parametros para obtener los valores
def fMP(num):
    global mFun
    global contMemoriaMV

    cont = contMemoriaMV - 1

    if cont > 0:
        return mFun[num]
    return num


#Valida que los imputs tengan el valor apropiado
def validarInput(var,mem):

    if mem >= 12000:
        mem = mandarDir(mem)

    #Valid que sea valor float
    if re.match(r'[-]?\d+\.\d+', var) is not None:
        if mem >= 1000 and mem < 2000 or mem >= 4000 and mem < 5000:
            return float(var)
        print("ERROR, Input tiene tipo de valor diferente")
        sys.exit(1)
        #Valid que sea valor int
    elif re.match(r'[-]?[0-9][0-9]*', var) is not None:
        if mem >= 0 and mem < 1000 or mem >= 3000 and mem < 4000:
            return int(var)
        print("ERROR, Input tiene tipo de valor diferente")
        sys.exit(1)
    else:
        #Valid que sea valor char
        if mem >= 2000 and mem < 3000 or mem >= 5000 and mem < 6000:
            return str(var)
        print("ERROR, Input tiene tipo de valor diferente")
        sys.exit(1)


#Asigna el valor de la soperaciones (solo se utiliza para las comparaciones)
#Los signos arismeticos no se estan utilizando
def compararVar(op,izq,der):
    if op == '<':
        return izq < der
    if op == '>':
        return izq > der
    if op == '<=':
        return izq <= der
    if op == '>=':
        return izq >= der
    if op == '==':
        return izq == der
    if op == '&':
        return izq and der
    if op == '|':
        return izq or der
    if op == '+':
        return izq + der
    if op == '-':
        return izq - der
    if op == '*':
        return izq * der
    if op == '/':
        return izq / der

#checa si la direccion es un apuntador, y si asi es devuelve la memoria a la que apunta
def mandarDir(memoria):
    global contMemoriaMV
    global mFun

    #Primero checa si es un espacio de memoria temporal o no
    if contMemoriaMV > 0:
        #En caso de serlo busca la memoria del apuntador
        for i in mFun:
            if mFun[i] == memoria:
                numMem = i
        #Valida si es un apuntador y en caso de serlo regresa la memoria a la que apunta
        if numMem >= 12000:
            return mFun[i]
    #Valida si es un apuntador y en caso de serlo regresa la memoria a la que apunta
    if memoria >= 12000:
        return memoriaMV[mem][memoria]
    return memoria


#Valida que los valores que se van a operar tengan valor
#(En teoria nunca tendria que pasar esto)
def validarOp(num1,num2, apuntador):
    if num1 == None or num2 == None:
        print('ERROR, Valor None en operacion')
        sys.exit(1)