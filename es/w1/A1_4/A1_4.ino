// ##### Aufgabe 1.4 #####
// Digital I/O / Blinking LED
// Board connection pin of the LED
// ANODE (+) -> "longer leg" of the LED

uint8_t pin_led = 13;
uint8_t interruptpin_button = 3;


bool ledState = LOW;
bool ledIsBlinking = false;

// ################################################################################


void toggle_ledstate() {
  ledState = !ledState;
}

void setup() {
  pinMode(pin_led, OUTPUT);
  pinMode(interruptpin_button, INPUT_PULLUP);
  digitalWrite(pin_led, ledState);
  attachInterrupt(digitalPinToInterrupt(interruptpin_button), led_off, LOW);
}

void loop() {
  if (ledIsBlinking) {
    toggle_ledstate();
    digitalWrite(pin_led, ledState);
    delay(400);
    toggle_ledstate();
    digitalWrite(pin_led, ledState);
    delay(200);
  }
}

void led_off() {
  ledIsBlinking = !ledIsBlinking;
}
