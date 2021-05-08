let battery;function mintfitosCreateWidget(){let widget=document.createElement('div');widget.id='mintfit-os';widget.innerHTML=`<style>
    #mintfit-os
    {
        position: fixed;
        bottom: 1rem;
        right: 1rem;
        z-index: 999999999;
        background: #3b515c;
        padding: 1.3rem 1rem 1rem 1rem;
        color: #ffffff;
        transition: opacity .1s linear;
    }

    #mintfit-os:hover
    {
        opacity: 0.1;
    }

    #mintfit-os-time
    {
        font-size: 2rem;
        line-height: 0;
    }

    #mintfit-os-battery
    {
        position: relative;
        display: block;
        margin-top: .3rem;
        padding: .2rem;
        background: #ffffff;
        text-shadow: 1px 1px #000000;
        text-align: center;
        border: 1px solid #ffffff;
    }

    #mintfit-os-battery:after
    {
        content: "";
        position: absolute;
        top: calc(50% - 6px);
        right: -5px;
        width: 4px;
        height: 12px;
        background: #ffffff;
    }
    </style>`;widget.innerHTML+='<time id="mintfit-os-time">00:00</time>&nbsp;Uhr<br><span id="mintfit-os-battery">0h 0min</span>';document.body.appendChild(widget)}
function mintfitosInitBattery(batteryManager){battery=batteryManager;mintfitosUpdateBattery();battery.addEventListener('chargingchange',mintfitosUpdateBattery);battery.addEventListener('chargingtimechange',mintfitosUpdateBattery);battery.addEventListener('dischargingtimechange',mintfitosUpdateBattery);battery.addEventListener('levelchange',mintfitosUpdateBattery)}
function mintfitosUpdateBattery(){let element=document.getElementById('mintfit-os-battery');element.style.background='linear-gradient(90deg, rgba(76, 175, 80, 1) 0%, rgba(76, 175, 80, 1) '+(battery.level*100)+'%, rgba(255, 255, 255, 1) '+(battery.level*100)+'%, rgba(255, 255, 255, 1) 100%)';element.innerHTML=mintfitosFormatBattery(battery.dischargingTime)}
function mintfitosUpdateTime(){let self=this;let now=new Date();document.getElementById('mintfit-os-time').innerHTML=mintfitosFormatTime(now.getHours())+':'+mintfitosFormatTime(now.getMinutes());setTimeout(mintfitosUpdateTime,60000)}
function mintfitosFormatTime(i){if(i<10){i='0'+i}
return i}
function mintfitosFormatBattery(duration){if(!isFinite(duration)){return'<span class="fa fa-plug"></span>'}else{let hours=Math.floor(duration/3600);let seconds=duration%60;let minutes=Math.floor((duration-hours*3600-seconds)/60);return hours+'h '+minutes+'min'}}
if(navigator.userAgent=="mintfit-os"){mintfitosCreateWidget();mintfitosUpdateTime();navigator.getBattery().then(mintfitosInitBattery,null);let navigationDrawer=document.querySelector('body.drawer-open-right .nav-link[data-action=toggle-drawer]');if(navigationDrawer){navigationDrawer.click()}}