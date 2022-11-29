# Percent of Source PIDs Correctly Predicted (Current Right Panel of Figure 4):
pids_correct = [59.45290, 52.17112]


#Overall Predicted Percent of List w/Characteristic (Current left panel of Figure 4):
percent_list_correct_labels = ['Positive','Extreme','Traits','Issues','Groups']
percent_list_correct = [
    [50.06492,49.02469], # human, gpt3
    [38.64391,39.89766],
    [72.60619,66.78702],
    [38.22335,41.52327],
    [48.59649,56.43027]
    ]

# Here are the updated Positivity percentages based on the ideology of the writer and their target:

positivity_labels = ["Extremely conservative","Conservative","Slightly conservative","Moderate","Slightly liberal","Liberal","Extremely Liberal"]

positivity = {}
positivity['Reps'] = [
[0.74,0.65],
[0.57,0.59],
[0.52,0.47],
[0.44,0.48],
[0.39,0.4],
[0.32,0.35],
[0.32,0.34],
]
positivity['Dems'] = [
[0.5,0.35],
[0.45,0.49],
[0.47,0.51],
[0.49,0.58],
[0.59,0.56],
[0.69,0.59],
[0.69,0.66],
]

extremity = {}
extremity['Reps'] = [
[0.28,0.36],
[0.35,0.32],
[0.24,0.3],
[0.35,0.38],
[0.52,0.46],
[0.5,0.51],
[0.5,0.64],
]
extremity['Dems'] = [
[0.78,0.41],
[0.45,0.36],
[0.29,0.27],
[0.41,0.35],
[0.34,0.37],
[0.3,0.31],
[0.3,0.27],
]
