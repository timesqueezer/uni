#include <TimerThree.h>

static const uint8_t led_pin = 13; // the pin with a LED
volatile bool led_state = LOW;

uint8_t button_pin = 3;
uint8_t button2_pin = 4;

bool toggle_complete = false;
bool toggle2_complete = false;

bool current_button_state = HIGH;
bool current_button2_state = HIGH;

const uint16_t TIMER_INTERVAL = 1E3; // 1000us
const uint16_t BUTTON_CHECK_THRESHOLD = 10000; // 10ms
const uint16_t button_total_checks = BUTTON_CHECK_THRESHOLD / TIMER_INTERVAL;

uint8_t button_check_count = 0;
uint8_t button_high_count = 0;

uint8_t button2_check_count = 0;
uint8_t button2_high_count = 0;

void changeLedState(void) {
  led_state = !led_state; //change of led state
}

void checkButtons(void) {
  bool current_state = digitalRead(button_pin);
  bool current_state2 = digitalRead(button2_pin);
  if (current_state == HIGH) {
    button_high_count++;
  }
  button_check_count++;

  if (current_state2 == HIGH) {
    button2_high_count++;
  }
  button2_check_count++;

  if (button_check_count >= button_total_checks) {
    if (current_button_state == HIGH) {
      bool button_is_low = (button_high_count / button_total_checks) < 0.5f;
      if (button_is_low) {
        current_button_state = LOW;
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

  if (button2_check_count >= button2_total_checks) {
    if (current_button2_state == HIGH) {
      bool button_is_low = (button2_high_count / button2_total_checks) < 0.5f;
      if (button_is_low) {
        current_button2_state = LOW;
      }
    } else {
      bool button_is_high = (button2_high_count / button2_total_checks) > 0.5f;
      if (button_is_high) {
        current_button2_state = HIGH;
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
  pinMode(button2_pin, INPUT);
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
