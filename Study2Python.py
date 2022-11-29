import pandas as pd
import numpy as np
from tqdm import tqdm
import os
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression, LogisticRegression

##To generate Appendix Figure 6

datasets = ["anes2016", "anes2012", "anes2020"]

models = [
    "j1-jumbo",
    "gpt3-davinci",
    "gpt3-curie",
    "j1-large",
    "gpt3-babbage",
    "gpt3-ada",
    "gpt-j",
    "gpt-neo-2.7B",
    "gpt-neo-1.3B",
    "gpt-neo-125M",
    "gpt2-xl",
    "gpt2-large",
    "gpt2-medium",
]
# models = ['j1-jumbo', 'gpt3-davinci', 'j1-large', 'gpt-j', 'gpt-neo-2.7B', 'gpt-neo-1.3B', 'gpt2-xl', ]

# B is billion, M is million
model_map = {
    "j1-jumbo": "Jurassic: 178B",
    "gpt3-davinci": "GPT-3: 175B",
    "gpt3-curie": "GPT-3: 13B",
    "j1-large": "Jurassic: 7.5B",
    "gpt3-babbage": "GPT-3: 6.7B",
    "gpt3-ada": "GPT-3: 2.7B",
    "gpt-j": "GPT-J: 6B",
    "gpt-neo-2.7B": "GPT-Neo: 2.7B",
    "gpt-neo-1.3B": "GPT-Neo: 1.3B",
    "gpt-neo-125M": "GPT-Neo: 125M",
    "gpt2-xl": "GPT-2: 1.5B",
    "gpt2-large": "GPT-2: 774M",
    "gpt2-medium": "GPT-2: 355M",
    "gpt2": "GPT-2: 124M",
}

param_counts = {
    "j1-jumbo": 178e9,
    "gpt3-davinci": 175e9,
    "gpt3-curie": 13e9,
    "j1-large": 7.5e9,
    "gpt3-babbage": 6.7e9,
    "gpt3-ada": 2.7e9,
    "gpt-j": 6e9,
    "gpt-neo-2.7B": 2.7e9,
    "gpt-neo-1.3B": 1.3e9,
    "gpt-neo-125M": 125e6,
    "gpt2-xl": 1.5e9,
    "gpt2-large": 774e6,
    "gpt2-medium": 355e6,
    "gpt2": 124e6,
}

# to model type
model_type = {
    "j1-jumbo": "Jurassic",
    "gpt3-davinci": "GPT-3",
    "gpt3-curie": "GPT-3",
    "j1-large": "Jurassic",
    "gpt3-babbage": "GPT-3",
    "gpt3-ada": "GPT-3",
    "gpt-j": "GPT-J",
    "gpt-neo-2.7B": "GPT-Neo",
    "gpt-neo-1.3B": "GPT-Neo",
    "gpt-neo-125M": "GPT-Neo",
    "gpt2-xl": "GPT-2",
    "gpt2-large": "GPT-2",
    "gpt2-medium": "GPT-2",
    "gpt2": "GPT-2",
}

dataset_map = {
    "anes2016": "ANES_2016",
    "anes2012": "ANES_2012",
    "anes2020": "ANES_2020",
}


def check_files_present():
    """
    Check if all files are present.
    """
    present = True
    for dataset in datasets:
        for model in models:
            file_name = get_file(dataset, model)
            if file_name is None:
                present = False
                print(f"No file found for {model} on {dataset}")
    if present:
        print("All files present")
    else:
        raise Exception("Some files missing")


def get_file(dataset, model):
    path = f"Study2_Data/{dataset}"
    # get all filenames in path
    files = os.listdir(path)
    try:
        # get the file with the model name in it AND '_processed.pkl' in it
        files = [f for f in files if model in f and "_processed.pkl" in f]
        # if model is gpt2, filter so 'gpt2_' is only thing included
        if model == "gpt2":
            files = [f for f in files if "gpt2_" in f]
        file_path = files[0]
        # if length of files is more than 1, print the files and raise a warning
        if len(files) > 1:
            print(f"Multiple files found for {model} on {dataset}")
            print(files)
            print()
    except:
        # if no file found, return None and raise warning
        print(f"No file found for {model} on {dataset}")
        return None
    return os.path.join(path, file_path)


def prep_scatter():
    """
    For each dataset and model, get the file and read in the df. Then, aggregate by 'template_name' and take the mean of 'accuracy' and 'mutual_inf' columns.
    """
    print("Prepping data file for plots")
    global models
    global model_type
    loop = tqdm(total=len(datasets) * len(models))
    # make empty df with points. Columns are models, rows are datasets
    dataset_dicts = []
    for dataset in datasets:
        model_dicts = []
        for model in models:
            file_name = get_file(dataset, model)
            print(file_name)
            exp_df = pd.read_pickle(file_name)
            if dataset == "anes2016":
                # keep only where ground_truth == 'clinton' or 'trump'
                exp_df = exp_df[
                    (exp_df["ground_truth"] == "clinton")
                    | (exp_df["ground_truth"] == "trump")
                ]
            elif dataset == "anes2020":
                # keep only where ground_truth == 'biden' or 'trump'
                exp_df = exp_df[
                    (exp_df["ground_truth"] == "biden")
                    | (exp_df["ground_truth"] == "trump")
                ]
            elif dataset == "anes2012":
                # keep only where ground_truth == 'obama' or 'romney'
                exp_df = exp_df[
                    (exp_df["ground_truth"] == "obama")
                    | (exp_df["ground_truth"] == "romney")
                ]
            # aggregate by 'template_name' and take the mean of 'accuracy' and 'mutual_inf' columns
            exp_df = exp_df.groupby("template_name").agg(
                {
                    "accuracy": np.mean,
                    "correct_weight": np.mean,
                    "mutual_inf": np.mean,
                    "version": "count",
                }
            )
            # change version to count
            exp_df.rename(columns={"version": "count"}, inplace=True)
            # add param_count column
            exp_df["param_count"] = param_counts[model]
            # add model_type column
            exp_df["model_type"] = model_type[model]
            # make 'template_name' (index) a column
            # exp_df.reset_index(inplace=True)
            # add to df
            model_dicts.append(exp_df)
            # increment loop
            loop.update(1)
        # add
        dataset_dicts.append(model_dicts)
    dataset_names = [dataset_map[d] for d in datasets]
    model_names = [model_map[m] for m in models]
    # make df with datasets as rows and models as columns
    df = pd.DataFrame(dataset_dicts, index=dataset_names, columns=model_names)
    # save to data/plot_data.pkl
    # df.to_pickle("data/pa/plot_data.pkl")
    # print("Saved to data/pa/plot_data.pkl")
    # if plots doesn't exist,
    # make plots directory
    #if not os.path.exists("plots"):
        #os.makedirs("plots")

    # %%
    # get all indices
    indices = df.index.values
    for index in indices:
        # make a new df with the model as the index, and accuracy, correct_weight, mutual_inf, and param_count as columns
        models = list(df.columns)
        d = {
            m: df.loc[index, m].loc["first_person_backstory"].to_dict() for m in models
        }
        # to anes df
        anes_df = pd.DataFrame(d).T

        # plot accuracy vs param_count
        # plt.figure(figsize=(10, 6))
        # color by model type
        model_types = anes_df["model_type"].unique()
        # for each model type, plot and color
        for model_type in model_types:
            df_model_type = anes_df[anes_df.model_type == model_type]
            #
            # plt.scatter(df_model_type['param_count'], df_model_type['accuracy'], label=model_type)
            # instead of scatter, do line plot with a dot for each point
            plt.scatter(
                df_model_type["param_count"],
                df_model_type["accuracy"],
                label=model_type,
                marker="o",
            )
        # log scale for param
        plt.xscale("log")
        # make y lim from .4 to 1
        plt.ylim(0.4, 0.95)
        plt.xlabel("param count (log)")
        plt.ylabel("accuracy")
        plt.legend()
        plt.title(index)
        plt.savefig(f"Figures/Appendix_Figure6_{index}.pdf")
        plt.close()


##To generate Appendix Figure 5

# sigmoid function
def sigmoid(x):
    return 1 / (1 + np.exp(-x))


# TODO - do correlation analysis between templates and ground truth. Add in functions for this?
def compare_per_template(df):
    group = df.groupby(by="template_name")

    output_df = group[["accuracy", "mutual_inf"]].agg(np.mean)

    corr = output_df.corr().iloc[0, 1]

    x, y = output_df.mutual_inf.values, output_df.accuracy.values

    plt.scatter(
        x=x,
        y=y,
        alpha=0.7,
        s=50,
        edgecolors="none",
    )

    # fit linear regression
    lr = LinearRegression()
    a, b = lr.fit(x.reshape(-1, 1), y).coef_[0], lr.intercept_
    # plot line
    x_linspace = np.linspace(x.min(), x.max(), 100)
    plt.plot(x_linspace, a * x_linspace + b, "C1", alpha=0.7)

    plt.title(f"Grouped by Template, Corr Coeff: {corr:.3f}")
    plt.xlabel(r"Mutual Information: $I(Y, f_{\theta}(X))$")
    plt.ylabel("Accuracy")

    return corr


def compare_per_response(df, y_jitter=0.05):
    corr = df[["accuracy", "mutual_inf"]].corr().iloc[0, 1]

    x, y = df.mutual_inf.values, df.accuracy.values

    plt.scatter(
        x=x,
        y=y + np.random.normal(0, y_jitter, len(y)),
        alpha=0.2,
        s=20,
        edgecolors="none",
    )

    # fit logistic regression
    lr = LogisticRegression()
    a, b = lr.fit(x.reshape(-1, 1), y).coef_[0], lr.intercept_
    # plot sigmoid regression
    x_linspace = np.linspace(x.min(), x.max(), 100)
    plt.plot(x_linspace, sigmoid(a * x_linspace + b), "C1", alpha=0.7)

    plt.title(f"Entropy Difference vs. Response Accuracy, Corr Coeff: {corr:.3f}")
    plt.xlabel(r"Entropy Difference: $H(Y) - H(Y|f_{\theta}(x_i))$")
    plt.ylabel("Accuracy")

    return corr


def compare_per_response_weight(df):
    corr = df[["correct_weight", "mutual_inf"]].corr().iloc[0, 1]

    x, y = df.mutual_inf.values, df.correct_weight.values

    # unique_template_names = list(set(df.template_name.values))
    # template_name_map = dict(zip(unique_template_names,
    #                          range(len(unique_template_names))))

    plt.scatter(
        x=x,
        y=y,
        alpha=0.2,
        s=20,
        edgecolors="none",
        # c=[template_name_map[v] for v in df.template_name.values]
    )

    # fit linear regression
    lr = LinearRegression()
    a, b = lr.fit(x.reshape(-1, 1), y).coef_[0], lr.intercept_
    # plot line
    x_linspace = np.linspace(x.min(), x.max(), 100)
    plt.plot(x_linspace, a * x_linspace + b, "C1", alpha=0.7)

    plt.title(f"Weight of Correct Response, Corr Coeff: {corr:.3f}")
    plt.xlabel(r"Entropy Difference: $H(Y) - H(Y|f_{\theta}(x_i))$")
    plt.ylabel("Weight on Correct")

    return corr


def compare_per_idx(df):
    group = df.groupby(by="raw_idx")

    output_df = group[["accuracy", "mutual_inf"]].agg(np.mean)

    corr = output_df.corr().iloc[0, 1]

    x, y = output_df.mutual_inf.values, output_df.accuracy.values

    plt.scatter(
        x=x,
        y=y,
        alpha=0.7,
        s=50,
        edgecolors="none",
    )

    # fit linear regression
    lr = LinearRegression()
    a, b = lr.fit(x.reshape(-1, 1), y).coef_[0], lr.intercept_
    # plot line
    x_linspace = np.linspace(x.min(), x.max(), 100)
    plt.plot(x_linspace, a * x_linspace + b, "C1", alpha=0.7)

    plt.title(f"Grouped by Instance, Corr Coeff: {corr:.3f}")
    plt.xlabel(
        r"Mean Entropy Difference: $\mathbb{E}_{\theta}[H(Y) - H(Y|f_{\theta}(x_i))]$"
    )
    plt.ylabel("Accuracy")

    return corr


def plot_comparisons(df, show=True, save=False, filename=None):
    """
    Calculates four different comparisons and shows or saves the results based
    on user input.
    """
    corrs = {}
    # # make figure big
    # plt.figure(figsize=(14,6))

    # plt.subplot(121)
    # corrs['per_template'] = compare_per_template(df)

    # plt.subplot(122)
    # corrs['per_response_weight'] = compare_per_response_weight(df)

    plt.figure(figsize=(14, 8))
    plt.subplot(221)
    corrs["per_template"] = compare_per_template(df)

    plt.subplot(222)
    corrs["per_response"] = compare_per_response(df)

    plt.subplot(223)
    corrs["per_response_weight"] = compare_per_response_weight(df)

    plt.subplot(224)
    corrs["per_id"] = compare_per_idx(df)

    # make suptitle with dataset
    plt.suptitle(f"{df.dataset.unique()[0].upper()} - {df.model.unique()[0].upper()}")

    plt.tight_layout()

    if save:
        if filename is None:
            raise ValueError("filename needs to be specified if save is True")
        plt.savefig(filename)
    if show:
        plt.show()
    else:
        plt.cla()

    return corrs


def get_sorted_templates(df):
    group = df.groupby(by="template_name")

    # agg accuracy and conditional entropy by mean, and prompt by first
    output_df = group.agg(
        {
            "accuracy": "mean",
            "mutual_inf": "mean",
            "coverage": "mean",
            "prompt": "first",
        }
    )

    # sort by conditional entropy
    output_df = output_df.sort_values(by="mutual_inf", ascending=True)
    return output_df

def agg_prob_dicts(dicts):
    """
    Given a list of probability dictionaries, aggregate them.
    """
    n = len(dicts)
    agg_dict = {}
    for d in dicts:
        for k, v in d.items():
            if k not in agg_dict:
                agg_dict[k] = v / n
            else:
                agg_dict[k] += v / n
    return agg_dict


def calculate_accuracy(df):
    """
    Calculates the accuracy of the model. Adds a column called 'accuracy' to df.
    df (pandas.DataFrame): dataframe with columns 'template_name', and 'ground_truth'

    Returns modified df.
    """
    df = df.copy()

    # if row['ground_truth'] starts with argmax(row['probs']) stripped and lowercase, then it's correct
    def accuracy_lambda(row):
        # guess is argmax of row['probs'] dict
        guess = max(row["probs"], key=row["probs"].get)
        # lower and strip
        guess = guess.lower().strip()
        if row["ground_truth"].lower().strip().startswith(guess):
            return 1
        else:
            return 0

    df["accuracy"] = df.apply(accuracy_lambda, axis=1)

    return df


def ensemble(df):
    """
    Aggregate 'probs' column with agg_prob_dicts, then fill in accuracy column.
    """
    # groupby 'raw_idx'
    df_grouped = df.groupby("raw_idx")
    # aggregate 'probs' column with agg_prob_dicts, and 'ground_truth' column with first
    df_agg = df_grouped.agg(
        {"probs": agg_prob_dicts, "ground_truth": lambda x: x.iloc[0]}
    )
    df_agg = calculate_accuracy(df_agg)
    return df_agg


def get_sorted_templates(df):
    group = df.groupby(by="template_name")

    # agg accuracy and conditional entropy by mean, and prompt by first
    output_df = group.agg(
        {
            "accuracy": "mean",
            "mutual_inf": "mean",
            "coverage": "mean",
            "prompt": "first",
        }
    )

    # sort by conditional entropy
    output_df = output_df.sort_values(by="mutual_inf", ascending=True)
    return output_df


def get_avg_acc(df):
    return df["accuracy"].mean()


def get_ensemble_acc(df, k):
    """
    Looks at a top k ensemble and returns the accuracy.
    """
    templates = get_sorted_templates(df)
    top_k_templates = templates.iloc[-k:].index.to_list()
    # filter df to only include top k templates
    df_top_k = df[df["template_name"].isin(top_k_templates)]
    top_k_acc = ensemble(df_top_k)["accuracy"].mean()
    return top_k_acc


def get_accuracies(df):
    """
    Returns a list of accuracies.
        First: average accuracy of all prompts
        Second: accuracy of ensemble of all prompts
        Third: accuracy of top 5 mutual information prompts
    """
    # first
    avg_acc = df["accuracy"].mean()
    # second
    ensemble_acc = ensemble(df)["accuracy"].mean()
    # third
    templates = get_sorted_templates(df)
    top_k_templates = templates.iloc[-5:].index.to_list()
    # filter df to only include top 5 templates
    df_top_k = df[df["template_name"].isin(top_k_templates)]
    top_k_acc = ensemble(df_top_k)["accuracy"].mean()
    return [avg_acc, ensemble_acc, top_k_acc]

def plot_ablations():
    df = pd.read_pickle('./Study2_Data/ablation_data.pkl')

    labels = []
    accuracies = []

    indices = df.index.values
    for index in indices:

        models = list(df.columns)
        d = {m: df.loc[index, m].loc['first_person_backstory'].to_dict() for m in models}

        anes_df = pd.DataFrame(d).T

        model_types = anes_df['model_type'].unique()

        for model_type in model_types:
            df_model_type = anes_df[anes_df.model_type == model_type]

            labels.append( index.replace("ANES 2016 ","") )
            accuracies.append( float(df_model_type['accuracy']) )

    for foo in zip( labels, accuracies ):
        print( foo[0], foo[1] )

    plt.bar( range(len(accuracies)), accuracies )
    plt.xticks( range(len(accuracies)), labels, rotation=-90 )
    plt.ylabel('Accuracy')
    plt.title("Ablation accuracies")

    plt.tight_layout()

    plt.savefig( './Figures/Appendix_Figure5_ablation_accuracy.pdf' )
    plt.close()
    


if __name__ == "__main__":
    check_files_present()
    prep_scatter()
    plot_ablations()
