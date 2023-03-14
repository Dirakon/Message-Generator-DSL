import { generateOneMessage,initializeVariables } from '../output/Main/index.js'

// source: string
export function compileSourceCode(source){ 
    return initializeVariables(source)
}

// compiledCode: output of compileSourceCode
export function generateMessage(compiledCode){ 
    return generateOneMessage(compiledCode)()
}

