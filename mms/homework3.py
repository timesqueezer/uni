import numpy as np
import matplotlib.pyplot as plt


"""
a)
It looks like the number of correct detections is in general higher than the false alarms.
This tells us that the detector is at least better than a blind guess, which would look like
a straight line from (0,0) to (1,1).
An ideal detector would just be a point at (0, 1), so with no false positives and only true positives.

b)
With mu_x=2 the detector performs even better, while mu_x=0 pushes the curve into the middle.
The higher the mu_x value is, the easier the detection problem is to solve.
This is because higher mu_x values have a higher chance to be above the threshold tau which all values are compared against.

c)
The curve looks like an exponential function and has very high false alarm and very low correct detection values.
This is a bad detector.
"""

num_samples = 10000

for mu_x in [1, 0, 2, -3]:
    # Start with only noise and add signal X depending on Pr(H_1) = 0.5
    # and keep track of whether the sample belongs to H_1
    v_samples = np.random.normal(loc=0, scale=1.0, size=num_samples)
    y_samples = []
    h_1_sample_indices = []

    for i, v in enumerate(v_samples):
        if np.random.random() < 0.5:
            y_samples.append(v + np.random.normal(loc=mu_x, scale=1.0))
            h_1_sample_indices.append(i)

        else:
            y_samples.append(v)

    def detector(sample, tau):
        if sample > tau:
            return 1

        else:
            return 0

    roc_points = []

    for tau in np.linspace(-5, 5):
        correct_detections = 0
        correct_total = 0
        false_alarms = 0
        false_total = 0

        for i, s in enumerate(y_samples):
            detected_value = detector(s, tau)
            correct_value = 1 if i in h_1_sample_indices else 0

            if detected_value == 1 and correct_value == 1:
                correct_detections += 1

            elif detected_value == 1 and correct_value == 0:
                false_alarms += 1

            if correct_value == 0:
                false_total += 1

            else:
                correct_total += 1

        if correct_total != 0 and false_total != 0:
            roc_points.append((
                false_alarms / false_total,
                correct_detections / correct_total,
            ))

    plt.plot(
        [rp[0] for rp in roc_points],
        [rp[1] for rp in roc_points],
        label=f'mu_x = {mu_x}'
    )

plt.xlabel('False Alarms')
plt.ylabel('Correct Detections')
plt.title('ROC')

plt.xlim(0, 1)
plt.ylim(0, 1)

plt.legend()

plt.show()
