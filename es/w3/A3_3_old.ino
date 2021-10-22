#include <TimerThree.h>

uint8_t button_pin = 3;
uint8_t button2_pin = 4;

bool toggle_complete = false;
bool toggle2_complete = false;
bool toggle_both_complete = false;

const uint16_t TIMER_INTERVAL = 1E3; // 1000us
const uint32_t BUTTON_CHECK_THRESHOLD = 25000; // 25ms
const uint16_t button_total_checks = BUTTON_CHECK_THRESHOLD / TIMER_INTERVAL;

uint8_t button_check_count = 0;
uint8_t button_high_count = 0;

uint8_t button2_check_count = 0;
uint8_t button2_high_count = 0;

uint8_t waiting_for_button2_counter = 0;
uint8_t waiting_for_button_counter = 0;

uint8_t button_counter = 0;
uint8_t button2_counter = 0;
uint8_t double_counter = 0;


void resetCheckButton() {
  waiting_for_button = false;
  waiting_for_button2 = false;

  button_high_count = 0;
  button_check_count = 0;

  button2_high_count = 0;
  button2_check_count = 0;

  waiting_for_button_counter = 0;
  waiting_for_button2_counter = 0;

  Timer3.detachInterrupt();
}

void checkButton() {
  bool current_state = digitalRead(button_pin);

  if (!waiting_for_button2) {
    if (current_state == HIGH) {
      button_high_count++;
    }
    button_check_count++;

    if (button_check_count >= button_total_checks) {
      float ratio = ((float) button_high_count / (float) button_total_checks);
      button_is_low = ratio < 0.2f;

      if (button_is_low) {
        waiting_for_button2 = true

      }
    }
  } else {
    // waiting for button2
    waiting_for_button2_counter++

    if (waiting_for_button2_counter >= button_total_checks) {
      // determine whether both buttons were pressed, otherwise trigger single button press
      if (button2_is_low) {
        toggle_both_complete = true
        resetCheckButton()
      } else {
        toggle_complete = true
        resetCheckButton()
      }
    }
  }

  bool current_state2 = digitalRead(button2_pin);

  if (!waiting_for_button) {
    if (current_state2 == HIGH) {
      button2_high_count++;
    }
    button2_check_count++;

    if (button2_check_count >= button_total_checks) {
      float ratio = ((float) button2_high_count / (float) button_total_checks);
      button2_is_low = ratio < 0.2f;

      if (button2_is_low) {
        waiting_for_button = true

      }
    }
  } else {
    // waiting for button 1
    waiting_for_button_counter++

    if (waiting_for_button_counter >= button_total_checks) {
      // determine whether both buttons were pressed, otherwise trigger single button press
      if (button_is_low) {
        toggle_both_complete = true
        resetCheckButton()
      } else {
        toggle2_complete = true
        resetCheckButton()
      }
    }
  }
}

void startButtonTimer() {
  //attatch ISR
  Timer3.attachInterrupt(checkButton); // attatch ISR & start timer

  Serial.println("Attaching button timer. Total checks: " + (String) button_total_checks);
}

void setup(void) {
  pinMode(button_pin, INPUT);
  pinMode(button2_pin, INPUT);

  Serial.begin(9600);

  //configuration of timer3 (@ 1kHz, 1E3us)
  Timer3.initialize(TIMER_INTERVAL);

  attachInterrupt(digitalPinToInterrupt(button_pin), startButtonTimer, FALLING);
  attachInterrupt(digitalPinToInterrupt(button2_pin), startButtonTimer, FALLING);

}

void printButtonCounters() {
  Serial.println(
    "(" + (String) button_counter +
    ", " + (String) button2_counter +
    ", " + (String) double_counter + ")"
  );
}

void loop(void) {
  // Checking against these specific values ensures that we completed the whole toggle process
  if (toggle_complete) {
    button_counter++;
    toggle_complete = false;
    /*changeLedState();
    digitalWrite(led_pin , led_state); */
    printButtonCounters();
  }

  if (toggle2_complete) {
    button2_counter++;
    toggle2_complete = false;
    printButtonCounters();
  }

  if (toggle_both_complete) {
    double_counter++;
    toggle_both_complete = false;
    printButtonCounters();
  }

}
