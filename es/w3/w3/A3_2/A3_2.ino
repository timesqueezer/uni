#include <TimerThree.h>

static const uint8_t led_pin = 13; // the pin with a LED
volatile bool led_state = LOW;

uint8_t button_pin = 3;
bool toggle_complete = false;
bool current_button_state = HIGH;
const uint16_t TIMER_INTERVAL = 1E3; // 1000us
const uint16_t BUTTON_CHECK_THRESHOLD = 10000; // 10ms
const uint16_t button_total_checks = BUTTON_CHECK_THRESHOLD / TIMER_INTERVAL;
uint8_t button_check_count = 0;
uint8_t button_high_count = 0;

void changeLedState(void) {
  led_state = !led_state; //change of led state
}

void checkButton(void) {
  bool current_state = digitalRead(button_pin);
  if (current_state == HIGH) {
    button_high_count++;
  }
  button_check_count++;

  if (button_check_count >= button_total_checks) {
    if (current_button_state == HIGH) {
      bool button_is_low = (button_high_count / button_total_checks) < 0.5f;
      if (button_is_low) {
        current_button_state = LOW;
        toggle_complete = true;
      }
    } else {
      bool button_is_high = (button_high_count / button_total_checks) > 0.5f;
      if (button_is_high) {
        current_button_state = HIGH;
        toggle_complete = true;
      }
    }

    button_high_count = 0;
    button_check_count = 0;
  }
}

void setup(void) {
  //configuration of digital I/O pin #13
  pinMode(led_pin , OUTPUT);
  pinMode(button_pin, INPUT);
  //configuration of timer3 (@ 1kHz, 1E3us)
  Timer3.initialize(TIMER_INTERVAL);
  //attatch ISR
  Timer3.attachInterrupt(checkButton); // attatch ISR & start timer
}

void loop(void) {
  // Checking against these specific values ensures that we completed the whole toggle process
  if (toggle_complete) {
    toggle_complete = false;
    changeLedState();
    digitalWrite(led_pin , led_state);
  }

}



/*

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
}*/
