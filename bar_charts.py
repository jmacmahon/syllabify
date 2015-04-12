import numpy as np
import matplotlib.pyplot as plt
import matplotlib
matplotlib.rc('font', family='DejaVu Sans')

def plot_confidences(data, xlabel='NO X-LABEL', ylabel='NO Y-LABEL', title='NO TITLE'):
    def process(datapoint):
        mean = (datapoint[1] + datapoint[2])/2.0
        error = datapoint[2] - mean
        return (datapoint[0], mean, error)

    width = 0.8
    processed = list(map(process, data))
    ticks, means, errors = zip(*processed)
    xs = np.arange(len(ticks))

    _, ax = plt.subplots()

    ax.bar(xs, means, yerr=errors, color=[(0, 0, 1, 0.6)], width=width)
    ax.set_xticks(xs + (width/2.0))
    ax.set_xticklabels(ticks)
    ax.set_ylabel(ylabel)
    ax.set_xlabel(xlabel)
    ax.set_title(title)
    plt.show()

heaviness_data = [
    ("real",0.5780582769640773,0.6042965582833199),
    ("elicited",0.4087061257332331,0.5601520057546561),
    ("carroll_lear",0.46103367578738086,0.629095079577426),
    #("hitchhikers",0.565721230518094,0.7058442646895738),
    ("dahl_rowling",0.4858149031084149,0.6488789744426056),
    #("gullivers",0.6849464092232138,0.8302051059283013)
]

syll_length_data = [
    ("real",2.1032170257159004,2.183417434831604),
    ("elicited",2.554360447222412,3.2256395527775883),
    ("carroll_lear",2.3397294522827585,2.8380483254950195),
    #("hitchhikers",2.160308378421013,2.5821158640032293),
    ("dahl_rowling",2.447438083422968,2.821379120878107),
    #("gullivers",2.142137349751413,2.524529316915254)
]

phoneme_length_data = [
    ("real",5.768491229223256,5.9453343976458495),
    ("elicited",6.916328896062727,8.423671103937272),
    ("carroll_lear",6.594580497871176,7.872086168795491),
    #("hitchhikers",6.305255972264079,7.361410694402587),
    ("dahl_rowling",7.0316197910938865,8.151175907830845),
    #("gullivers",6.771538757742425,7.854723868520201)
]

cvratio_data = [
    ("real",1.4248074742125865,1.4832561054866042),
    ("elicited",1.6810747399096506,2.237925260090349),
    ("carroll_lear",1.7720214984235707,2.3516822052801327),
    #("hitchhikers",1.568760853448213,2.017097732410373),
    ("dahl_rowling",1.7183016771196913,2.2630603300487673),
    #("gullivers",1.7125212873468219,2.1474113725858377),
]

onset_consonants_data = [
    ("real",1.0308407574515865,1.0597463413999753),
    ("elicited",1.0919391006965575,1.2748429062238575),
    ("carroll_lear",1.1246869119119993,1.2873302554699748),
    #("hitchhikers",1.1623700798693668,1.3232529233255215),
    ("dahl_rowling",1.1740552930111368,1.3647202171929447),
    #("gullivers",1.280256725501989,1.4903060450607815)
]

coda_consonants_data = [
    ("real",0.5018523334427767,0.5360353986598314),
    ("elicited",0.309425433922086,0.4795019017180524),
    ("carroll_lear",0.4018862508274429,0.6024055946661194),
    #("hitchhikers",0.48320247939744565,0.6861265940849826),
    ("dahl_rowling",0.4407780135143674,0.6367730068937959),
    #("gullivers",0.562097277718528,0.7539200382987881)
]

schwa_data = [
    ("real",0.18938881910995606,0.21073997771305567),
    ("elicited",0.11870836747522169,0.23423280899536658),
    ("carroll_lear",8.279327197377426e-2,0.20046853060133304),
    #("hitchhikers",0.10089271886060061,0.20581654631511823),
    ("dahl_rowling",9.897849242203943e-2,0.21938885451673606),
    #("gullivers",5.557458101987428e-2,0.16087563543034217)
]

clusters_data = [
    ("real",0.10573421446726129,0.11762094277219347),
    ("elicited",8.766849525272485e-2,0.1580062452317042),
    ("carroll_lear",9.626749396621197e-2,0.17841061762177085),
    #("hitchhikers",0.1350810199926168,0.21316179150898068),
    ("dahl_rowling",0.12025625299386947,0.20627435925102847),
    #("gullivers",0.15324699946074566,0.24935040313665696)
]

cross_clusters_data = [
    ("real",0.23585314420766293,0.26756737893117005),
    ("elicited",0.1629834184441425,0.32378906832834425),
    ("carroll_lear",0.2146548426714494,0.41471578669917997),
    #("hitchhikers",0.18564670610096762,0.3557897579874301),
    ("dahl_rowling",0.2989069814303861,0.5037245975169824),
    #("gullivers",0.3578044516147934,0.5815894877791461)
]

def test():
    def mean_plot_confidences(data, yword):
        plot_confidences(data, xlabel='Dataset', ylabel='Mean %s' % yword, title='')
    graphs = [
        (syll_length_data, 'syllable length'),
        (phoneme_length_data, 'phoneme length'),
        (cvratio_data, 'consonant-vowel ratio'),
        (onset_consonants_data, 'number of consonants in onsets'),
        (coda_consonants_data, 'number of consonants in codas'),
        (schwa_data, 'proportion of nucleuses filled by schwa'),
        (heaviness_data, 'proportion of heavy syllables'),
        (clusters_data, 'proportion of onsets/codas containing clusters'),
        (cross_clusters_data, 'proportion of cross-consonant clusters in syllable boundaries')
    ]
    for (data, word) in graphs:
        mean_plot_confidences(data, word)
