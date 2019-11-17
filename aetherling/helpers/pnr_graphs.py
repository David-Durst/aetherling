import pandas as pd
from math import isnan, nan
from fractions import Fraction as frac
import matplotlib.pyplot as plt
import os
import matplotlib.ticker as ticker

def plot_from_results_str(results_file):
    results = pd.read_csv(results_file)
    results['Clock Rate'] = nan
    results_tex_str = ""
    systems = ["aetherling_copies", "halide_to_hardware", "spatial"]
    systb = {"ae": 0, "h2h": 1, "sp": 2}
    applications = ["map", "conv2d", "conv2d_b2b", "conv2d_b2b_3x3_repeat", "pyramid", "sharpen", "camera"]
    apptb = {"map": 0, "conv2d": 1, "conv2d_b2b": 2, "sharpen": 5}
    application_lengths = [200, 16, 16, 16, 64, 16, 200]
    index_of_p_1_row_ae = [3, 2, 2, 2, 2, 2, 3]
    index_of_p_1_row_other = 0
    application_parallelisms = [[frac(1,8), frac(1,4), frac(1,2) , frac(1,1),frac(2,1),frac(4,1),frac(5,1),frac(8,1),frac(10,1),frac(20,1),frac(200,1)],
                                [frac(1,9), frac(1,3), frac(1,1), frac(2,1),frac(4,1),frac(8,1),frac(16,1)],
                                [frac(1,9), frac(1,3), frac(1,1), frac(2,1),frac(4,1),frac(8,1),frac(16,1)],
                                [frac(1,9), frac(1,3), frac(1,1), frac(2,1),frac(4,1),frac(8,1),frac(16,1)],
                                [frac(1,9), frac(1,3), frac(1,1), frac(2,1),frac(4,1),frac(8,1),frac(16,1),frac(32,1),frac(64,1)],
                                [frac(1,9), frac(1,3), frac(1,1), frac(2,1),frac(4,1),frac(8,1),frac(16,1)],
                                [frac(1,8), frac(1,4), frac(1,2) , frac(1,1),frac(2,1),frac(4,1),frac(5,1),frac(10,1),frac(20,1),frac(200,1)]]
    application_parallelisms_others = [[frac(1,1), frac(2,1), frac(4,1), frac(8, 1)],
                                       [frac(1,1), frac(2,1), frac(4,1), frac(8, 1)],
                                       [frac(1,1), frac(2,1), frac(4,1), frac(8, 1)],
                                       [frac(1,1), frac(2,1), frac(4,1), frac(8, 1)],
                                       [frac(1,1), frac(2,1), frac(4,1), frac(8, 1)],
                                       [frac(1,1), frac(2,1), frac(4,1), frac(8, 1)],
                                       [frac(1,1), frac(2,1), frac(4,1), frac(8, 1)]]
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
#    per_system_results = [results[results.System == system] for system in systems]
#    per_system_per_application = \
#        [[per_system_result[per_system_result.Application == app]
#          for app in applications]
#         for per_system_result in per_system_results]
    fig, ((ax0_0, ax0_1, ax0_2, ax0_3), (ax1_0, ax1_1, ax1_2, ax1_3)) = plt.subplots(nrows=2, ncols=4)
    plt.rc('text', usetex=True)
    fig.set_figwidth(20)
    fig.set_figheight(12)
    # map
    ax0_0.set_title('Map')
    ax0_0.set_yscale('log')
    ax0_0.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax0_0.set_xscale('log')
    ax0_0.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax0_0.set_xticks([1,2,5,10,20,40,200])
    res = per_system_per_application_results
    res[systb['h2h']][apptb['map']].plot(kind='line', y="LUTs", x="Parallelism",
                                         ax=ax0_0, label="Halide-To-Hardware",
                                         linestyle='--', marker='o' )
    print("plotting map h2h")
    print(res[systb['h2h']][apptb['map']])
    res[systb['sp']][apptb['map']].plot(kind='line', y="LUTs", x="Parallelism",
                                        ax=ax0_0, label="Spatial",
                                        linestyle='--', marker='o')
    print("plotting map sp")
    print(res[systb['sp']][apptb['map']])
    res[systb['ae']][apptb['map']].plot(kind='line', y="LUTs", x="Parallelism",
                                        ax=ax0_0, label="Aetherling",
                                        linestyle='--', marker='o')
    print("plotting map ae")
    print(res[systb['ae']][apptb['map']])
    ax0_0.set_ylabel('LUTs')
    ax0_0.set_xlabel('Input Px Per Clock');

    #conv2d
    ax0_1.set_title('Single 3x3 Conv')
    ax0_1.set_yscale('log')
    ax0_1.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax0_1.set_xscale('log')
    ax0_1.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax0_1.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax0_1.set_xticks([1/9,1/3])
    #ax0_1.set_xticklabels([r'$\frac{1}{3}$'])
    ax0_1.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res = per_system_per_application_results
    res[systb['h2h']][apptb['conv2d']].plot(kind='line', y="LUTs", x="Parallelism",
                                         ax=ax0_1, label="Halide-To-Hardware",
                                         linestyle='--', marker='o' )
    print("plotting map h2h")
    print(res[systb['h2h']][apptb['conv2d']])
    res[systb['sp']][apptb['conv2d']].plot(kind='line', y="LUTs", x="Parallelism",
                                        ax=ax0_1, label="Spatial",
                                        linestyle='--', marker='o')
    print("plotting conv2d sp")
    print(res[systb['sp']][apptb['conv2d']])
    res[systb['ae']][apptb['conv2d']].plot(kind='line', y="LUTs", x="Parallelism",
                                        ax=ax0_1, label="Aetherling",
                                        linestyle='--', marker='o')
    print("plotting conv2d ae")
    print(res[systb['ae']][apptb['conv2d']])
    ax0_1.set_ylabel('LUTs')
    ax0_1.set_xlabel('Input Px Per Clock');


    figs_dir = os.path.join(os.path.dirname(results_file), 'figs')
    plt.savefig(os.path.join(figs_dir, 'test.pdf'))

    #conv2d_b2b
    ax0_2.set_title('3x3 Conv to 2x2 Conv')
    ax0_2.set_yscale('log')
    ax0_2.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax0_2.set_xscale('log')
    ax0_2.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax0_2.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax0_2.set_xticks([1/9,1/3])
    #ax0_2.set_xticklabels([r'$\frac{1}{3}$'])
    ax0_2.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res = per_system_per_application_results
    res[systb['h2h']][apptb['conv2d_b2b']].plot(kind='line', y="LUTs", x="Parallelism",
                                            ax=ax0_2, label="Halide-To-Hardware",
                                            linestyle='--', marker='o' )
    print("plotting map h2h")
    print(res[systb['h2h']][apptb['conv2d_b2b']])
    res[systb['sp']][apptb['conv2d_b2b']].plot(kind='line', y="LUTs", x="Parallelism",
                                           ax=ax0_2, label="Spatial",
                                           linestyle='--', marker='o')
    print("plotting conv2d_b2b sp")
    print(res[systb['sp']][apptb['conv2d_b2b']])
    res[systb['ae']][apptb['conv2d_b2b']].plot(kind='line', y="LUTs", x="Parallelism",
                                           ax=ax0_2, label="Aetherling",
                                           linestyle='--', marker='o')
    print("plotting conv2d_b2b ae")
    print(res[systb['ae']][apptb['conv2d_b2b']])
    ax0_2.set_ylabel('LUTs')
    ax0_2.set_xlabel('Input Px Per Clock');

    #sharpen
    ax0_3.set_title('Sharpen')
    ax0_3.set_yscale('log')
    ax0_3.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax0_3.set_xscale('log')
    ax0_3.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax0_3.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax0_3.set_xticks([1/9,1/3])
    #ax0_3.set_xticklabels([r'$\frac{1}{3}$'])
    ax0_3.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res = per_system_per_application_results
    res[systb['h2h']][apptb['sharpen']].plot(kind='line', y="LUTs", x="Parallelism",
                                                ax=ax0_3, label="Halide-To-Hardware",
                                                linestyle='--', marker='o' )
    print("plotting map h2h")
    print(res[systb['h2h']][apptb['sharpen']])
    res[systb['sp']][apptb['sharpen']].plot(kind='line', y="LUTs", x="Parallelism",
                                               ax=ax0_3, label="Spatial",
                                               linestyle='--', marker='o')
    print("plotting sharpen sp")
    print(res[systb['sp']][apptb['sharpen']])
    res[systb['ae']][apptb['sharpen']].plot(kind='line', y="LUTs", x="Parallelism",
                                               ax=ax0_3, label="Aetherling",
                                               linestyle='--', marker='o')
    print("plotting sharpen ae")
    print(res[systb['ae']][apptb['sharpen']])
    ax0_3.set_ylabel('LUTs')
    ax0_3.set_xlabel('Input Px Per Clock');


    ax1_0.set_title('Map')
    ax1_0.set_yscale('log')
    ax1_0.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_0.set_xscale('log')
    ax1_0.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_0.set_xticks([1,2,5,10,20,40,200])
    res = per_system_per_application_results
    res[systb['h2h']][apptb['map']].plot(kind='line', y="Slices", x="Parallelism",
                                         ax=ax1_0, label="Halide-To-Hardware",
                                         linestyle='--', marker='o' )
    print("plotting map h2h")
    print(res[systb['h2h']][apptb['map']])
    res[systb['sp']][apptb['map']].plot(kind='line', y="Slices", x="Parallelism",
                                        ax=ax1_0, label="Spatial",
                                        linestyle='--', marker='o')
    print("plotting map sp")
    print(res[systb['sp']][apptb['map']])
    res[systb['ae']][apptb['map']].plot(kind='line', y="Slices", x="Parallelism",
                                        ax=ax1_0, label="Aetherling",
                                        linestyle='--', marker='o')
    print("plotting map ae")
    print(res[systb['ae']][apptb['map']])
    ax1_0.set_ylabel('Slices')
    ax1_0.set_xlabel('Input Px Per Clock');

    #conv2d
    ax1_1.set_title('Single 3x3 Conv')
    ax1_1.set_yscale('log')
    ax1_1.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_1.set_xscale('log')
    ax1_1.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_1.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_1.set_xticks([1/9,1/3])
    #ax1_1.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_1.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res = per_system_per_application_results
    res[systb['h2h']][apptb['conv2d']].plot(kind='line', y="Slices", x="Parallelism",
                                            ax=ax1_1, label="Halide-To-Hardware",
                                            linestyle='--', marker='o' )
    print("plotting map h2h")
    print(res[systb['h2h']][apptb['conv2d']])
    res[systb['sp']][apptb['conv2d']].plot(kind='line', y="Slices", x="Parallelism",
                                           ax=ax1_1, label="Spatial",
                                           linestyle='--', marker='o')
    print("plotting conv2d sp")
    print(res[systb['sp']][apptb['conv2d']])
    res[systb['ae']][apptb['conv2d']].plot(kind='line', y="Slices", x="Parallelism",
                                           ax=ax1_1, label="Aetherling",
                                           linestyle='--', marker='o')
    print("plotting conv2d ae")
    print(res[systb['ae']][apptb['conv2d']])
    ax1_1.set_ylabel('Slices')
    ax1_1.set_xlabel('Input Px Per Clock');


    figs_dir = os.path.join(os.path.dirname(results_file), 'figs')
    plt.savefig(os.path.join(figs_dir, 'test.pdf'))

    #conv2d_b2b
    ax1_2.set_title('3x3 Conv to 2x2 Conv')
    ax1_2.set_yscale('log')
    ax1_2.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_2.set_xscale('log')
    ax1_2.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_2.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_2.set_xticks([1/9,1/3])
    #ax1_2.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_2.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res = per_system_per_application_results
    res[systb['h2h']][apptb['conv2d_b2b']].plot(kind='line', y="Slices", x="Parallelism",
                                                ax=ax1_2, label="Halide-To-Hardware",
                                                linestyle='--', marker='o' )
    print("plotting map h2h")
    print(res[systb['h2h']][apptb['conv2d_b2b']])
    res[systb['sp']][apptb['conv2d_b2b']].plot(kind='line', y="Slices", x="Parallelism",
                                               ax=ax1_2, label="Spatial",
                                               linestyle='--', marker='o')
    print("plotting conv2d_b2b sp")
    print(res[systb['sp']][apptb['conv2d_b2b']])
    res[systb['ae']][apptb['conv2d_b2b']].plot(kind='line', y="Slices", x="Parallelism",
                                               ax=ax1_2, label="Aetherling",
                                               linestyle='--', marker='o')
    print("plotting conv2d_b2b ae")
    print(res[systb['ae']][apptb['conv2d_b2b']])
    ax1_2.set_ylabel('Slices')
    ax1_2.set_xlabel('Input Px Per Clock');

    #sharpen
    ax1_3.set_title('Sharpen')
    ax1_3.set_yscale('log')
    ax1_3.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_3.set_xscale('log')
    ax1_3.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_3.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_3.set_xticks([1/9,1/3])
    #ax1_3.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_3.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res = per_system_per_application_results
    res[systb['h2h']][apptb['sharpen']].plot(kind='line', y="Slices", x="Parallelism",
                                             ax=ax1_3, label="Halide-To-Hardware",
                                             linestyle='--', marker='o' )
    print("plotting map h2h")
    print(res[systb['h2h']][apptb['sharpen']])
    res[systb['sp']][apptb['sharpen']].plot(kind='line', y="Slices", x="Parallelism",
                                            ax=ax1_3, label="Spatial",
                                            linestyle='--', marker='o')
    print("plotting sharpen sp")
    print(res[systb['sp']][apptb['sharpen']])
    res[systb['ae']][apptb['sharpen']].plot(kind='line', y="Slices", x="Parallelism",
                                            ax=ax1_3, label="Aetherling",
                                            linestyle='--', marker='o')
    print("plotting sharpen ae")
    print(res[systb['ae']][apptb['sharpen']])
    ax1_3.set_ylabel('Slices')
    ax1_3.set_xlabel('Input Px Per Clock');




    figs_dir = os.path.join(os.path.dirname(results_file), 'figs')
    plt.savefig(os.path.join(figs_dir, 'test.pdf'))


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
