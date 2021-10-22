// ##### Aufgabe 1.3 #####
// Digital I/O / Blinking LED
// Board connection pin of the LED
// ANODE (+) -> "longer leg" of the LED

uint8_t pin_led = 13;
uint8_t pin_button = 3;


bool ledState = LOW;
bool changeStatus = false;
bool ledIsBlinking = false;
bool oldButtonState = HIGH;

// ################################################################################


void toggle_ledstate() {
  ledState = !ledState;
}

void check_button() {
  if (digitalRead(pin_button) == LOW && oldButtonState == HIGH) {
    changeStatus = true;
  }
  oldButtonState = digitalRead(pin_button);
}

void setup() {
  pinMode(pin_led, OUTPUT);
  pinMode(pin_button, INPUT);
  digitalWrite(pin_led, ledState);
}

void loop() {
  check_button();
  if (changeStatus) {
    ledIsBlinking = !ledIsBlinking;
    changeStatus = false;
  }
  if (ledIsBlinking) {
    toggle_ledstate();
    digitalWrite(pin_led, ledState); // 400 Ticks warten und zwischendurch Schalter prüfen
    for (int i = 0; i < 8; i++) {
      delay(50);
      check_button();
    }
    toggle_ledstate();
    digitalWrite(pin_led, ledState); // 200 Ticks warten und zwischendurch Schalter prüfen
    for (int i = 0; i < 4; i++) {
      delay(50);
      check_button();
    } 
  }
}
