import axios from 'axios'
import Cookies from 'js-cookie'


export default class InputRecorder {
    constructor() {
        this.eventList = []
        this.dataSenderTimeout = null
        this.userUUID = null

    }

    async init() {
        await this.setUserUUID()

        this.setupListeners()
        this.setupDataSender()
    }

    async setUserUUID() {
        const alreadySetUserUUID = Cookies.get('user_uuid')
        console.log({alreadySetUserUUID})
        if (alreadySetUserUUID) {
            this.userUUID = alreadySetUserUUID

        } else {
            const response = await axios.post('/user-uuid')
            this.userUUID = response.data.user_uuid
            Cookies.set('user_uuid', this.userUUID)

        }
    }

    setupListeners() {
        document.addEventListener('pointermove', this.onPointerMove.bind(this), { passive: true })
        document.addEventListener('pointerdown', this.onPointerDown.bind(this), { passive: true })
        document.addEventListener('pointerup', this.onPointerUp.bind(this), { passive: true })
    }

    setupDataSender() {
        this.dataSenderTimeout = window.setTimeout(this.sendData.bind(this), 1000)
    }

    async sendData() {
        console.log(this)
        const data = this.eventList
        this.eventList = []

        const payload = {
            meta: {
                user_uuid: this.userUUID,
            },
            data,
        }

        console.log(payload)

        await axios.post('/data-capture', payload)

        this.setupDataSender()
    }

    onPointerMove(e) {
        this.eventList.push({
            type: 'move',
            event: e.toString(),
            dt: (new Date()).toISOString(),
        })
    }

    onPointerDown(e) {
        console.log('onPointerDown', e)
        this.eventList.push({
            type: 'pointerdown',
            event: e.toString(),
            dt: (new Date()).toISOString(),
        })
    }

    onPointerUp(e) {
        console.log('onPointerUp', e)
    }
}
