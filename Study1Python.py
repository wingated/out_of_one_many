import os
import numpy as np
import pandas as pd
from tqdm import tqdm
import matplotlib.pyplot as plt
import seaborn as sns
import pickle
import copy


plt.ion()


##Plot figures 1 and 2 in the main paper

#
# =======================================================================
#

def make_df(csvpath=''):
    return pd.read_csv(os.path.join('.', csvpath))

def word_cleaner( word ):
    return str(word).lower().strip()

#
# =======================================================================
#

import re
pattern = re.compile('[^\w_\- ]+')

stop_words = set( 'a an the who are and to not of are i s nan in for is be re am for t na don that'.split() )

# accumulate a dataframe
def acc_df( df, htype, stats, demographic_map ):

    for ind in tqdm( range(df.shape[0]) ):
        row = df.iloc[ind]
        for ptype in ['D','R']:
            for wnum in range(4):

                if USE_PID:
                    pid = demographic_map[ row['ID'] ]['PID7']
                    col_ofs = pid_mapper[ pid ]
                else:
                    ideo = demographic_map[ row['ID'] ]['Ideo']
                    col_ofs = ideo_mapper[ ideo ]

                if col_ofs == -99:
                    continue

                phrase = row[htype+ptype+str(wnum+1)]

                cleanw = word_cleaner( phrase )
                cleanw = pattern.sub( '', cleanw )
                parts = cleanw.split()

                for p in parts:
                    if len(p) == 0:
                        continue
                    if p in stop_words:
                        continue

                    if not p in stats:
                        stats[p] = np.zeros((1,28))

                    if ptype=='D' and htype=='G':
                        colind = 0
                    if ptype=='D' and htype=='H':
                        colind = 7
                    if ptype=='R' and htype=='G':
                        colind = 14
                    if ptype=='R' and htype=='H':
                        colind = 21

                    colind += col_ofs

                    stats[p][0,colind] += 1

#
# =======================================================================
#

def make_bubbles( cntmat, cntlist, words, CNT=50, yofs=0, xofs=0, SKIP=1, plot_words=False, alpha=0.5 ):
    WIDTH=cntmat.shape[1]
    slist = sorted( enumerate(cntlist), key=lambda item: item[1], reverse=True )
    topmat = np.zeros( (CNT,WIDTH) )
    swords = []
    for topind, wind in enumerate(slist[0:CNT]):
        topmat[topind,:] = cntmat[ wind[0], : ]
        swords.append( words[ wind[0] ] )

    tmp = []
    for ind in range(CNT):
        tmp.append(np.atleast_2d(list(range(WIDTH)))+1.0)
    xmat = np.vstack( tmp )
    tmp = []
    for ind in range(WIDTH):
        tmp.append( np.atleast_2d(list(range(CNT)))+1.0 )
    ymat = CNT-np.vstack( tmp ).T

    plt.scatter( xmat.ravel()+xofs, ymat.ravel()+yofs, s=topmat.ravel(), alpha=alpha )

    if plot_words:
        for wind,w in enumerate(swords[0::SKIP]):
            plt.text( 0, CNT-(SKIP*wind)+yofs-1, w, horizontalalignment='right', verticalalignment='center', fontfamily="serif" )

#
# =======================================================================
#

USE_PID = True  # if False, use Ideo

pid_mapper = {
    'Strong D':6,
    'Weak D':5,
    'Lean D':4,
    'Ind':3,
    'Lean R':2,
    'Weak R':1,
    'Strong R':0,
    np.nan:-99,
}

ideo_mapper = {
    'Extremely Liberal':6,
    'Liberal':5,
    'Slightly liberal':4,
    "Moderate/Haven't thought about it":3,
    'Slightly conservative':2,
    'Conservative':1,
    'Extremely conservative':0,
    np.nan:-99,
}

gptdf = make_df('Study1_Data/gpt3_uber_final.csv')
ppdf = make_df('Study1_Data/ppfull.csv')

demographic_map = {}
for ind in tqdm( range(ppdf.shape[0]) ):
    hrow = ppdf.iloc[ind]
    demographic_map[ hrow['ID'] ] = {
        'ID':hrow['ID'],
        'Ideo':hrow['Ideo'],
        'Gender':hrow['Gender'],
        'PID7':hrow['PID7'],
        'Inc':hrow['Inc']
        }

cnts = np.zeros((1,7))
if USE_PID:
    for k in demographic_map.keys():
        r = demographic_map[k]
        pid = pid_mapper[ r['PID7'] ]
        if pid != -99:
            cnts[0,pid] += 1
else:
    for k in demographic_map.keys():
        r = demographic_map[k]
        pid = ideo_mapper[ r['Ideo'] ]
        if pid != -99:
            cnts[0,pid] += 1

propcor = np.hstack( [1/cnts,1/cnts,1/cnts,1/cnts] )

# ---------------------------------------------------


ideos = ppdf['Ideo'].unique()
pid7s = ppdf['PID7'].unique()

stats = {}
acc_df( gptdf, 'G', stats, demographic_map )
acc_df( ppdf, 'H', stats, demographic_map )


words = list( stats.keys() )

cntmat = np.zeros((len(words),28))
for ind,w in enumerate(words):
    cntmat[ind,:] = stats[w]
total_cnts = np.sum( cntmat, axis=1, keepdims=True )

dem_cnts = np.sum( cntmat[:,0:14], axis=1 )
rep_cnts = np.sum( cntmat[:,14:28], axis=1 )

orig_cntmat = copy.copy(cntmat)

propcor = 1/np.sum(cntmat,axis=0,keepdims=True)
propcor = 28*propcor/np.sum(propcor)
#cntmat = total_cnts * ( cntmat * propcor )
cntmat = cntmat * propcor

#
# =======================================================================
#

gpt3_color = [166/255,206/255,227/255]
human_color = [31/255,120/255,180/255]
sns.set_palette( [ gpt3_color, human_color, gpt3_color, human_color ], 4 )

plt.rcParams['figure.figsize'] = [8,6]
SKIP=1
SF=3
CNT=25

plt.figure()
make_bubbles( SF*cntmat[:,0:7], dem_cnts, words, yofs=0, xofs=0, SKIP=SKIP, CNT=CNT, plot_words=True )
make_bubbles( SF*cntmat[:,7:14], dem_cnts, words, yofs=0, xofs=8, SKIP=SKIP, CNT=CNT )
make_bubbles( SF*cntmat[:,14:21], dem_cnts, words, yofs=0, xofs=16, SKIP=SKIP, CNT=CNT )
make_bubbles( SF*cntmat[:,21:28], dem_cnts, words, yofs=0, xofs=24, SKIP=SKIP, CNT=CNT )
plt.gca().axis('off')
plt.savefig("Figures/Main_Figure2_dem_output.pdf")


plt.figure()
make_bubbles( SF*cntmat[:,0:7], rep_cnts, words, yofs=0, xofs=0, SKIP=SKIP, CNT=CNT, plot_words=True )
make_bubbles( SF*cntmat[:,7:14], rep_cnts, words, yofs=0, xofs=8, SKIP=SKIP, CNT=CNT )
make_bubbles( SF*cntmat[:,14:21], rep_cnts, words, yofs=0, xofs=16, SKIP=SKIP, CNT=CNT )
make_bubbles( SF*cntmat[:,21:28], rep_cnts, words, yofs=0, xofs=24, SKIP=SKIP, CNT=CNT )
plt.gca().axis('off')

plt.savefig("Figures/Main_Figure2_rep_output.pdf")


##Plot Figure 3 in the main paper:
 
import sys
# adding Data to the system path
sys.path.insert(0, 'Study1_Data/')
from fig3_data import *

plt.figure()

plt.subplot(2,5,1)
plt.plot( positivity['Reps'] )
plt.title("Republicans")
plt.ylim([0,1])

plt.subplot(2,5,2)
plt.plot( positivity['Dems'] )
plt.title("Democrats")
plt.ylim([0,1])

plt.subplot(2,5,6)
plt.plot( extremity['Reps'] )
plt.ylim([0,1])

plt.subplot(2,5,7)
plt.plot( extremity['Dems'] )
plt.ylim([0,1])

plt.subplot(2,5,3)
plt.bar( [0,1], np.atleast_1d(pids_correct)/100.0, 0.5 );
plt.title( "Source PIDs" )
plt.ylim([0,1])

plot_inds = [4,5,8,9,10]
for ind, plot_ind in enumerate(plot_inds):
    plt.subplot(2,5,plot_ind)
    plt.bar( [0,1], np.atleast_1d(percent_list_correct[ind])/100.0, 0.5 )
    plt.title( percent_list_correct_labels[ind] )
    plt.ylim([0,1])

plt.savefig("Figures/Main_Figure3.pdf")
