import {main} from '../index.js'
import {readFileSync} from  'fs'
import {Client} from 'tdl'
import {TDLib} from 'tdl-tdlib-addon'
import { fileURLToPath } from 'url'
import path from 'path'
import readline from 'readline'
import dotenv from 'dotenv'
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

dotenv.config();

const inquirer = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  

main()

let allOptions = JSON.parse( readFileSync("out.txt").toString())


const client = new Client(new TDLib(path.join(__dirname, process.env.TDLIB_COMMAND)), {
  apiId: parseInt(process.env.APP_ID), // Your api_id, get it at http://my.telegram.org/
  apiHash: process.env.APP_HASH // Your api_hash
  
})

client.on('error', console.error)
client.on('update', update => {
 // console.log('Received update:', update)
})

async function startTelegram () {
  await client.connectAndLogin()

  let curUnixTime = Math.floor(new Date().getTime() / 1000)
  await client.invoke({
    _: 'sendMessage',
    chat_id: parseInt(process.env.CHAT_ID),
    options: {
      _: 'messageSendOptions',
      disable_notification: false,
      from_background :false,
      scheduling_state: {
        _: 'messageSchedulingStateSendAtDate',
        send_date:curUnixTime + 604800
      }
    },
    input_message_content: {
      _: 'inputMessageText',
      text: {
        _: 'formattedText',
        text: '👻!'
      }
    }
  })
  //
  // ...
}

startTelegram().catch(console.error)
    