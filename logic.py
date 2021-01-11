def designer(op, izq, der):

    tiposVal = {'izq':  ['int'  ,'int'  ,'int' ,'float','float','float','char','char' ,'char'],
                'der':  ['int'  ,'float','char','int'  ,'float','char' ,'int' ,'float','char'], 
                '+':    ['int'  ,'float','err' ,'float','float','err'  ,'err' ,'err'  ,'char'],
                '-':    ['int'  ,'float','err' ,'float','float','err'  ,'err' ,'err'  ,'err' ],
                '*':    ['int'  ,'float','err' ,'float','float','err'  ,'err' ,'err'  ,'err' ],
                '/':    ['float','float','err' ,'float','float','err'  ,'err' ,'err'  ,'err' ],
                '=':    ['int'  ,'err'  ,'err' ,'float','float','err'  ,'err' ,'err'  ,'char']}
    
    if op == '!=' or op == '==':
        return 'bool'

    if op == '<' or op == '>' or op == '<=' or op == '>=':
        if (izq == 'char' and der != 'char') or (izq != 'char' and der == 'char'):
            return 'err'
        if izq == 'bool' or der =='bool':
            return 'err'
        return 'bool'
    
    if (op == '&' or op =='|') and (izq != 'bool' or der != 'bool'):
        return 'err'
    elif op == '&' or op =='|':
        return 'bool'


    for i in range(len(tiposVal['izq'])):
        if tiposVal['izq'][i] == izq and tiposVal['der'][i] == der:
            return tiposVal[str(op)][i]
    
    return 'err'

