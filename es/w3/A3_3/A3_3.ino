#include <TimerThree.h>

static const uint8_t led_pin = 13; // the pin with a LED
volatile bool led_state = LOW;

uint8_t button_pin = 3;
uint8_t button2_pin = 4;

bool toggle_complete = false;
bool toggle2_complete = false;
bool toggle_both_complete = false;

bool current_button_state = HIGH;
bool current_button2_state = HIGH;

const uint16_t TIMER_INTERVAL = 1E3; // 1000us
const uint32_t BUTTON_CHECK_THRESHOLD = 50000; // 50ms
const uint16_t button_total_checks = BUTTON_CHECK_THRESHOLD / TIMER_INTERVAL;

uint8_t button_check_count = 0;
uint8_t button_high_count = 0;

uint8_t button2_check_count = 0;
uint8_t button2_high_count = 0;

bool check_for_double_high = false;

bool button_counting = false;
bool button2_counting = false;

uint8_t button_counter = 0;
uint8_t button2_counter = 0;
uint8_t double_counter = 0;

void changeLedState(void) {
  led_state = !led_state; //change of led state
}

void checkButtons(void) {
  bool button_is_high = false;
  bool button2_is_high = false;

  bool current_state = digitalRead(button_pin);
  bool current_state2 = digitalRead(button2_pin);

  if (!button_counting && current_state != current_button_state) { // start counting
    button_counting = true;
  }

  if (!button2_counting && current_state2 != current_button2_state) { // start counting for button2
    button2_counting = true;
  }

  if (button_counting) {
    if (current_state == HIGH) {
      button_high_count++;
    }
    button_check_count++;

    if (button_check_count >= button_total_checks) {
      if (current_button_state == HIGH) {
        float ratio = ((float) button_high_count / (float) button_total_checks);
        Serial.println("Ratio button1 falling: " + (String) ratio);
        bool button_is_low = ratio < 0.2f;
        if (button_is_low) {
          current_button_state = LOW;
        } else {
          button_counting = false;
        }
      } else {
        float ratio = ((float) button_high_count / (float) button_total_checks);
        button_is_high = ratio > 0.8f;
        Serial.println("Ratio button1 rising: " + (String) ratio);
        if (button_is_high) {
          if (!check_for_double_high) {
            current_button_state = HIGH;
            toggle_complete = true;
            button_counting = false;
          } else {
            if (button2_is_high) {
              current_button_state = HIGH;
              current_button2_state = HIGH;
              toggle_both_complete = true;
              check_for_double_high = false;
              button_counting = false;
            }
          }
        }
      }
  
      button_high_count = 0;
      button_check_count = 0;
    }

  }

  if (button2_counting) {
    if (current_state2 == HIGH) {
      button2_high_count++;
    }
    button2_check_count++;

    if (button2_check_count >= button_total_checks) {
      if (current_button2_state == HIGH) {
        float ratio = ((float) button2_high_count / (float) button_total_checks);
        Serial.println("Ratio button2 falling: " + (String) ratio);
        bool button_is_low = ratio < 0.2f;
        if (button_is_low) {
          current_button2_state = LOW;
        } else {
          button2_counting = false;
        }
      } else {
        float ratio = ((float) button2_high_count / (float) button_total_checks);
        button2_is_high = ratio > 0.8f;
        Serial.println("Ratio button2 rising: " + (String) ratio);
        if (button2_is_high) {
          if (!check_for_double_high) {
            current_button2_state = HIGH;
            toggle2_complete = true;
            button2_counting = false;
          } else {
            if (button_is_high) {
              current_button_state = HIGH;
              current_button2_state = HIGH;
              toggle_both_complete = true;
              check_for_double_high = false;
              button2_counting = false;
            }
          }
        }
      }

      button2_high_count = 0;
      button2_check_count = 0;
    }
  }

  if (current_button_state == LOW && current_button2_state == LOW) {
    check_for_double_high = true;
  }

}

void setup(void) {
  //configuration of digital I/O pin #13
  pinMode(led_pin , OUTPUT);
  pinMode(button_pin, INPUT);
  pinMode(button2_pin, INPUT);

  Serial.begin(9600);

  //configuration of timer3 (@ 1kHz, 1E3us)
  Timer3.initialize(TIMER_INTERVAL);
  //attatch ISR
  Timer3.attachInterrupt(checkButtons); // attatch ISR & start timer

  Serial.println("Checking " + (String) button_total_checks + " times per trigger.");
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
