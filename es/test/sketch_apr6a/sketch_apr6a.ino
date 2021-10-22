
uint8_t pin_led = 13; //Nr. des ext. Pins
bool ledState = LOW; //Zustandsvariable

void toggle_ledstate() {
	ledState = !ledState;
}

void setup()
 {
	pinMode(pin_led , OUTPUT);
	digitalWrite(pin_led , ledState);
 }

void loop()
 {
   toggle_ledstate();
   digitalWrite(pin_led , ledState);
   delay(400);
   toggle_ledstate();
   digitalWrite(pin_led , ledState);
   delay(200);
 }
