import pandas as pd
from math import isnan, nan
from fractions import Fraction as frac
import matplotlib.pyplot as plt
import os
import matplotlib.ticker as ticker

def plot_from_results_str(results_file, results_all_types_file):
    results = pd.read_csv(results_file)
    results_all_types = pd.read_csv(results_all_types_file)
    print("all types")
    print(results_all_types)
    results['Clock Rate'] = nan
    systems = ["aetherling_copies", "halide_to_hardware", "spatial"]
    systb = {"ae": 0, "h2h": 1, "sp": 2}
    applications = ["map", "conv2d", "conv2d_b2b", "conv2d_b2b_3x3_repeat", "pyramid", "sharpen", "camera"]
    apptb = {"map": 0, "conv2d": 1, "conv2d_b2b": 2, "sharpen": 5}
    apptb_cmp = {"map": 0, "conv2d": 1, "conv2d_b2b": 2, "sharpen": 3}
    application_lengths = [200, 16, 16, 16, 64, 16, 200]
    per_system_per_application_results = []
    for i, system in enumerate(systems):
        per_system_results = []
        for j, app in enumerate(applications):
            start_per_app_per_system = results[(results.System == system) & (results.Application == app)]
            paper_parallelism = fix_parallelism(start_per_app_per_system, application_lengths[j])
            #filled_in = add_missing_parallelisms(paper_parallelism, system, app, application_parallelisms[j] if i == 0 else application_parallelisms_others[j])
            sorted_by_parallelism = paper_parallelism.sort_values("Parallelism")
            results_only_selected_columns = get_output_columns(sorted_by_parallelism)
            per_system_results.append(results_only_selected_columns)
        per_system_per_application_results.append(per_system_results)
    per_application_all_types_results = []
    for j, app in enumerate(applications):
        start_per_system = results_all_types.loc[results_all_types.Application == app,:]
        paper_parallelism = fix_parallelism(start_per_system, application_lengths[j])
        #filled_in = add_missing_parallelisms(paper_parallelism, system, app, application_parallelisms[j] if i == 0 else application_parallelisms_others[j])
        sorted_by_parallelism = paper_parallelism.sort_values("Parallelism")
        results_only_selected_columns = get_output_columns(sorted_by_parallelism)
        per_application_all_types_results.append(results_only_selected_columns)
#    per_system_results = [results[results.System == system] for system in systems]
#    per_system_per_application = \
#        [[per_system_result[per_system_result.Application == app]
#          for app in applications]
#         for per_system_result in per_system_results]
    fig, (ax1_0, ax1_1, ax1_2, ax1_3) = plt.subplots(nrows=1, ncols=4)
    plt.rc('text', usetex=True)
    fntsize = 20
    plt.rcParams.update({'font.size': fntsize})
    fig.set_figwidth(26)
    fig.set_figheight(7)
    x_label = "Input Throughput"
    y_label = "Area (Slices)"
    # map


    ax1_0.set_title('Map')
    ax1_0.set_yscale('log')
    ax1_0.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_0.set_xscale('log')
    ax1_0.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_0.set_xticks([1,2,5,10,20,40,200])
    res = per_system_per_application_results
    res_all_types = per_application_all_types_results
    res[systb['ae']][apptb['map']].plot(kind='line', y="Slices", x="Parallelism",
                                        ax=ax1_0, label="Scheduler Result", color=["g"],
                                        linestyle='--', marker='o', fontsize=fntsize)
    print("plotting map ae")
    print(res[systb['ae']][apptb['map']])
    print("plotting map ae all types")
    print(res_all_types[apptb['map']])
    res_all_types[apptb['map']].plot(kind='scatter', y="Slices", x="Parallelism",
                                        ax=ax1_0, label="Other Output Types", color=["black"],
                                        marker='s', fontsize=fntsize)
    ax1_0.set_ylabel(y_label, fontsize=fntsize)
    ax1_0.set_xlabel(x_label, fontsize=fntsize)

    #conv2d
    conv2d_title = 'Single 3x3 Conv'
    ax1_1.set_title(conv2d_title)
    ax1_1.set_yscale('log')
    ax1_1.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    #ax1_1.yaxis.set_minor_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_1.minorticks_off()
    ax1_1.set_yticks([60,100,300])
    ax1_1.set_xscale('log')
    ax1_1.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_1.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_1.set_xticks([1/9,1/3])
    #ax1_1.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_1.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res[systb['ae']][apptb['conv2d']].plot(kind='line', y="Slices", x="Parallelism",
                                           ax=ax1_1, label="Scheduler Result", color=["g"],
                                           linestyle='--', marker='o', fontsize=fntsize)
    print("plotting conv2d ae")
    print(res[systb['ae']][apptb['conv2d']])
    print("plotting conv2d ae all types")
    print(res_all_types[apptb['conv2d']])
    res_all_types[apptb['conv2d']].plot(kind='scatter', y="Slices", x="Parallelism",
                                           ax=ax1_1, label="Other Output Types", color=["black"],
                                           marker='s', fontsize=fntsize)
    ax1_1.set_ylabel(y_label, fontsize=fntsize)
    ax1_1.set_xlabel(x_label, fontsize=fntsize);



    #conv2d_b2b
    conv2d_b2b_title = '3x3 Conv to 2x2 Conv'
    ax1_2.set_title(conv2d_b2b_title)
    ax1_2.set_yscale('log')
    ax1_2.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    #ax1_2.yaxis.set_minor_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_2.minorticks_off()
    ax1_2.set_yticks([70,100,500])
    ax1_2.set_xscale('log')
    ax1_2.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_2.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_2.set_xticks([1/9,1/3])
    #ax1_2.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_2.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res[systb['ae']][apptb['conv2d_b2b']].plot(kind='line', y="Slices", x="Parallelism",
                                               ax=ax1_2, label="Scheduler Result", color=["g"],
                                               linestyle='--', marker='o', fontsize=fntsize)
    print("plotting conv2d_b2b ae")
    print(res[systb['ae']][apptb['conv2d_b2b']])
    res_all_types[apptb['conv2d_b2b']].plot(kind='scatter', y="Slices", x="Parallelism",
                                               ax=ax1_2, label="Other Output Types", color=["black"],
                                               marker='s', fontsize=fntsize)
    print("plotting conv2d_b2b ae")
    print(res_all_types[apptb['conv2d_b2b']])
    ax1_2.set_ylabel(y_label, fontsize=fntsize)
    ax1_2.set_xlabel(x_label, fontsize=fntsize);

    #sharpen
    sharpen_title = "Sharpen"
    ax1_3.set_title(sharpen_title)
    ax1_3.set_yscale('log')
    ax1_3.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    #ax1_3.yaxis.set_minor_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_3.minorticks_off()
    ax1_3.tick_params(axis='both', which='major', labelsize=fntsize)
    ax1_3.set_yticks([70,100,500])
    ax1_3.set_xscale('log')
    ax1_3.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_3.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_3.set_xticks([1/9,1/3])
    #ax1_3.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_3.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res[systb['ae']][apptb['sharpen']].plot(kind='line', y="Slices", x="Parallelism",
                                            ax=ax1_3, label="Scheduler Result", color=["g"],
                                            linestyle='--', marker='o', fontsize=fntsize)
    print("plotting sharpen ae")
    print(res[systb['ae']][apptb['sharpen']])
    res_all_types[apptb['sharpen']].plot(kind='scatter', y="Slices", x="Parallelism",
                                            ax=ax1_3, label="Other Output Types", color=["black"],
                                            marker='s', fontsize=fntsize)
    print("plotting sharpen ae")
    print(res_all_types[apptb['sharpen']])
    ax1_3.set_ylabel(y_label, fontsize=fntsize)
    ax1_3.set_xlabel(x_label, fontsize=fntsize);

    figs_dir = os.path.join(os.path.dirname(results_file), 'figs')
    plt.savefig(os.path.join(figs_dir, 'ae_results.pdf'))

    sp_maxes = []
    sp_mins = []
    hth_maxes = []
    hth_mins = []
    joined_res_list = []
    for app in apptb:
        print("App " + str(app))
        (joined_res, joined_sp_ratios, joined_hth_ratios) = \
            comp_ae_and_others(res[systb['ae']][apptb[app]], res[systb['h2h']][apptb[app]], res[systb['sp']][apptb[app]])
        joined_res_list.append(joined_res)
        print("Max SP" + str(joined_sp_ratios.iloc[:,1].max()))
        print("Min SP" + str(joined_sp_ratios.iloc[:,1].min()))
        print("Max HTH" + str(joined_hth_ratios.iloc[:,1].max()))
        print("Min HTH" + str(joined_hth_ratios.iloc[:,1].min()))
        sp_maxes.append(joined_sp_ratios.iloc[:,1].max())
        sp_mins.append(joined_sp_ratios.iloc[:,1].min())
        hth_maxes.append(joined_hth_ratios.iloc[:,1].max())
        hth_mins.append(joined_hth_ratios.iloc[:,1].min())
    print("sp_maxes: " + str(sp_maxes))
    print("sp_mins: " + str(sp_mins))
    print("hth_maxes: " + str(hth_maxes))
    print("hth_mins: " + str(hth_mins))





    fig, (ax2_0, ax2_1, ax2_2, ax2_3) = plt.subplots(nrows=1, ncols=4)
    plt.rc('text', usetex=True)
    fntsize = 20
    plt.rcParams.update({'font.size': fntsize})
    fig.set_figwidth(26)
    fig.set_figheight(7)

    def plot_bar_comp(axis, title, appname):
        axis.set_title(title)
        joined_res_list[apptb_cmp[appname]].fillna(0)
        joined_res_list[apptb_cmp[appname]].plot(kind='bar', y=["Aetherling", "Spatial", "Halide-HLS"], x="Parallelism", xticks=[1,2,4,8], rot=0,
                                              ax=axis, label=["Aetherling", "Spatial", "Halide-HLS"], color=["g","r","b"],
                                              fontsize=fntsize)
        axis.set_xticklabels([r'$1$',r'$2$',r'$4$',r'$8$'])
        print("plotting " + str(appname) + " ae")
        print(joined_res_list[apptb_cmp[appname]])
        axis.set_ylabel(y_label, fontsize=fntsize)
        axis.set_xlabel(x_label, fontsize=fntsize);

    plot_bar_comp(ax2_0, conv2d_title, 'map')
    plot_bar_comp(ax2_1, conv2d_title, 'conv2d')
    plot_bar_comp(ax2_2, conv2d_b2b_title, 'conv2d_b2b')
    plot_bar_comp(ax2_3, sharpen_title, 'sharpen')
    # conv2d
    #ax2_0.set_yscale('log')
    #ax2_0.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ##ax2_0.yaxis.set_minor_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    #ax2_0.minorticks_off()
    #ax2_0.set_yticks([60,100,600])
    #ax2_0.set_xscale('log')
    #ax2_0.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(int(x))))
    #ax2_0.set_xticks([1,2,4,6])
    ##ax2_0.set_xticks([1/9,1/3])
    ##ax2_0.set_xticklabels([r'$\frac{1}{3}$'])

    plt.savefig(os.path.join(figs_dir, 'ae_versus.pdf'))

    # get all Aetherling results into latex tables
    #aetherling_per_app = per_system_per_application_results[0]
    #for i, system_per_app in enumerate(per_system_per_application_results):
    #    for j, app_pd in enumerate(system_per_app):
    #        results_tex_str += "System {}, App {}\n".format(systems[i], applications[j])
    #        results_tex_str += app_pd.to_latex(index=False, escape=False)
    #for app_idx in range(len(applications)):
    #    results_tex_str += "Comparison for App {}\n".format(applications[app_idx])
    #    ae_res = per_system_per_application_results[0][app_idx]
    #    ae_res_for_comp = ae_res[ae_res.Parallelism.isin([int_if_not_nan(x) for x in application_parallelisms_others[0]])]
    #    results_tex_str += merge_columns(
    #        ae_res_for_comp,
    #        per_system_per_application_results[1][app_idx],
    #        per_system_per_application_results[2][app_idx],
    #    ).to_latex(index=False, escape=False)
    #return results_tex_str

def comp_ae_and_others(ae_pd, hth_pd, sp_pd):
    ae_pd = ae_pd.rename(columns={"Slices": "AE_Slices", "Parallelism": "AE_Parallelism"}).loc[:,["AE_Slices", "AE_Parallelism"]]
    hth_pd = hth_pd.rename(columns={"Slices": "HTH_Slices", "Parallelism": "HTH_Parallelism"}).loc[:,["HTH_Slices", "HTH_Parallelism"]]
    sp_pd = sp_pd.rename(columns={"Slices": "SP_Slices", "Parallelism": "SP_Parallelism"}).loc[:, ["SP_Slices", "SP_Parallelism"]]
    joined_sp = ae_pd.merge(sp_pd, left_on='AE_Parallelism', right_on='SP_Parallelism', how="inner")
    joined_hth = ae_pd.merge(hth_pd, left_on='AE_Parallelism', right_on='HTH_Parallelism', how="inner")
    joined_res = joined_sp.merge(hth_pd, left_on='AE_Parallelism', right_on='HTH_Parallelism', how="left")
    joined_sp.loc[:,'Parallelism'] = joined_sp.loc[:,'SP_Parallelism']
    joined_hth.loc[:,'Parallelism'] = joined_hth.loc[:,'HTH_Parallelism']
    joined_res.loc[:,'Parallelism'] = joined_res.loc[:,'AE_Parallelism']
    joined_sp.loc[:,'AE_SP_Slices_Ratio'] = joined_sp.loc[:,'SP_Slices'] / joined_sp.loc[:, 'AE_Slices']
    joined_hth.loc[:,'AE_HTH_Slices_Ratio'] = joined_hth.loc[:,'HTH_Slices'] / joined_hth.loc[:, 'AE_Slices']
    joined_sp_ratios = joined_sp.loc[:,['Parallelism', 'AE_SP_Slices_Ratio']]
    joined_hth_ratios = joined_hth.loc[:,['Parallelism', 'AE_HTH_Slices_Ratio']]
    joined_res = joined_res.loc[:, ['Parallelism', 'AE_Slices', 'SP_Slices', 'HTH_Slices']].rename(
        columns={"AE_Slices":"Aetherling","SP_Slices":"Spatial","HTH_Slices":"Halide-HLS"}
    )
    print(joined_sp_ratios)
    print(joined_hth_ratios)
    return (joined_res, joined_sp_ratios, joined_hth_ratios)

def add_missing_parallelisms(results_pd, system, application, parallelisms_to_add):
    for p in parallelisms_to_add:
        if p not in results_pd.Parallelism.values:
            results_pd = results_pd.append({"System": system, "Application":application, "Parallelism": p}, ignore_index=True)
    return results_pd

def fix_parallelism(results_pd, length):
    results_pd['Parallelism'] = results_pd['Parallelism'].apply(lambda x: length / x)#frac(length, x))
    return results_pd

def get_output_columns(results_pd):
    results_pd['LUTs'] = results_pd['TotalLUTs']
    results_pd['BRAMs'] = results_pd['RAMB36'] + results_pd['RAMB18']
    results_pd['Slices'] = results_pd['Slices']
    results_pd['Parallelism'] = results_pd['Parallelism']
    results_pd['MHz'] = results_pd['Slack(VIOLATED)'].apply(fix_clock)
    return results_pd.loc[:,['Parallelism', 'LUTs', 'BRAMs', 'Slices', 'MHz']]

def percent_vs_base(results_pd, column_name, index_of_p_1_row):
    #others = pd.to_numeric(other_results[column_name], errors='coerce')
    #base = pd.to_numeric(result_pd[column_name], errors='coerce')
    #results_pd[column_name + '_diff'] = pd.to_numeric(results_pd.loc[:,column_name], errors='coerse') - \
    #                                    results_pd.at[index_of_p_1_row, column_name]
    p_1_value = results_pd[column_name].iloc[index_of_p_1_row]
    def get_ratio(num):
        if num == "\\red{X}" or str(num) == "nan" or p_1_value == "\\red{X}" or str(p_1_value) == "nan" or \
                num == "0" or p_1_value == "0":
            return num
        else:
            return num + " " + "(" + str(round((float(num) / float(p_1_value)), 2)) + ")"
    results_pd[column_name] = results_pd[column_name].apply(get_ratio)
    return results_pd
    #return others.apply(int_if_not_nan)# ((others - ae) / ae).apply(int_if_not_nan)
    #return other_results[column_name].apply(int_if_not_nan) #others.apply(int_if_not_nan)# ((others - ae) / ae).apply(int_if_not_nan)

def merge_columns(aetherling_results, halide_results, spatial_results):
    aetherling_results['ALUTs'] = aetherling_results['LUTs']
    aetherling_results['ABRAMs'] = aetherling_results['BRAMs']
    aetherling_results['ASlices'] = aetherling_results['Slices']
    halide_results['HLUTs'] = halide_results['LUTs']#percent_vs_aetherling(aetherling_results, halide_results, 'LUTs')
    halide_results['HBRAMs'] = halide_results['BRAMs'] #percent_vs_aetherling(aetherling_results, halide_results, 'BRAMs')
    halide_results['HSlices'] = halide_results['Slices'] #percent_vs_aetherling(aetherling_results, halide_results, 'Slices')
    spatial_results['SLUTs'] = spatial_results['LUTs'] #percent_vs_aetherling(aetherling_results, spatial_results, 'LUTs')
    spatial_results['SBRAMs'] = spatial_results['BRAMs'] #percent_vs_aetherling(aetherling_results, spatial_results, 'BRAMs')
    spatial_results['SSlices'] = spatial_results['Slices'] #percent_vs_aetherling(aetherling_results, spatial_results, 'Slices')
    joined = pd.merge(pd.merge(aetherling_results, halide_results, on='Parallelism'), spatial_results, on='Parallelism')
    return joined[['Parallelism',
                               'HLUTs', 'HBRAMs', 'HSlices',
                               'SLUTs', 'SBRAMs', 'SSlices',
                               ]]

base_ns = 5.6

def fix_clock(x_str):
    if str(x_str) == "nan":
        return "\\red{X}"
    x = float(x_str[:-2])
    if x > 0:
        return str(round(1000 / base_ns))
    else:
        return str(round(1000 / (base_ns + -1 * x)))

def int_if_not_nan(x):
    if type(x) == str:
        return x
    elif isnan(x):
        return "\\red{X}"
    elif type(x) is int:
        return str(x)
    elif type(x) is float and x == int(x):
        return str(int(x))
    elif x.denominator == 1:
        return str(x.numerator)
    else:
        #return "$\\frac{" + str(fr.numerator) + "}{" + str(fr.denominator) + "}$"
        return str(x.numerator) + "/" + str(x.denominator)