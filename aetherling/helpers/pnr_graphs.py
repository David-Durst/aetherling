import pandas as pd
from math import isnan, nan
from fractions import Fraction as frac
import matplotlib.pyplot as plt
import os
import matplotlib.ticker as ticker
import matplotlib.colors as mcolors

pd.set_option('display.max_columns', 20)


def plot_from_results_str(results_file):
    results = pd.read_csv(results_file)
    print("all types")
    results['Clock Rate'] = nan
    systems = ["aetherling_copies", "halide_to_hardware", "spatial"]
    systb = {"ae": 0, "h2h": 1, "sp": 2}
    big_applications = ["big_conv2d", "big_conv2d_b2b", "big_sharpen"]
    big_16_applications = ["big_16_conv2d", "big_16_conv2d_b2b", "big_16_sharpen"]
    big_32_applications = ["big_32_conv2d", "big_32_conv2d_b2b", "big_32_sharpen"]
    big_real_applications = ["big_real_conv2d", "big_real_conv2d_b2b", "big_real_sharpen"]
    big_real_16_applications = ["big_real_16_conv2d", "big_real_16_conv2d_b2b", "big_real_16_sharpen"]
    big_real_32_applications = ["big_real_32_conv2d", "big_real_32_conv2d_b2b", "big_real_32_sharpen"]
    real_applications = ["conv2d_real", "conv2d_real_b2b", "sharpen_real"]
    all_big_apps = real_applications + \
                   big_applications + big_16_applications + big_32_applications + \
                   big_real_applications + big_real_16_applications + big_real_32_applications
    #applications = ["map", "conv2d", "conv2d_b2b", "conv2d_b2b_3x3_repeat", "pyramid", "sharpen", "camera"] + all_big_apps
    applications = ["map", "conv2d", "conv2d_b2b", "sharpen"] + all_big_apps
    applications_paper_just_ae = ["map"] + big_real_32_applications
    big_apptb = {name:idx+4 for (idx, name) in enumerate(all_big_apps)}
    apptb = {"map": 0, "conv2d": 1, "conv2d_b2b": 2, "sharpen": 3}
    apptb.update(big_apptb)
    application_lengths = [200, 16, 16, 16] + ([1920*1080] * 21)
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
    #fig, ((ax1_0, ax1_1, _), (ax1_2, ax1_3, _), (ax1_4, ax1_5, _)) = plt.subplots(nrows=17, ncols=3)
    fig, axes = plt.subplots(nrows=23, ncols=3, figsize=(24,130))
    (ax1_0, ax1_1, _) = axes[0]
    (ax1_2, ax1_3, _) = axes[1]
    big_axes = axes[2:]
    for axis in [ax1_0, ax1_1, ax1_2, ax1_3]:
        axis.spines['right'].set_visible(False)
        axis.spines['top'].set_visible(False)
    #plt.subplots_adjust(hspace=0.5, top=1.6)
    plt.rc('text', usetex=True)
    fntsize = 28
    plt.rcParams.update({'font.size': fntsize})
    #fig.set_figwidth(24)
    #fig.set_figheight(130)
    x_label = "Throughput (Input Px / Clk)"
    y_label = "Area (Slices)"
    ms = 18
    lw = 7
    # map


    map_title = "MAP"
    ax1_0.set_title(map_title)
    ax1_0.set_yscale('log')
    #ax1_0.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_0.yaxis.set_major_formatter(ticker.ScalarFormatter())
    ax1_1.tick_params(axis='both', which='both', labelsize=fntsize)
    ax1_0.set_xscale('log')
    ax1_0.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_0.set_xticks([1,2,5,10,20,40,200])
    res = per_system_per_application_results
    res[systb['ae']][apptb['map']].plot(kind='line', y="Slices", x="Parallelism", legend=False,
                                        ax=ax1_0, label="Scheduler Result", color=["g"],
                                        linestyle='-', marker='o', fontsize=fntsize,
                                        markersize=ms, linewidth=lw
                                        )
    print("plotting map ae")
    print(res[systb['ae']][apptb['map']])
    ax1_0.set_ylabel(y_label, fontsize=fntsize)
    ax1_0.set_xlabel("", fontsize=fntsize)

    #conv2d
    conv2d_title = 'CONV'
    ax1_1.set_title(conv2d_title)
    ax1_1.set_yscale('log')
    #ax1_1.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_1.yaxis.set_major_formatter(ticker.ScalarFormatter())
    #ax1_1.yaxis.set_minor_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_1.minorticks_off()
    #ax1_1.tick_params(axis='both', which='both', labelsize=fntsize)
    ax1_1.tick_params(axis='y', which='both', pad=20)
    ax1_1.set_yticks([50,100,300])
    ax1_1.set_yticks([], minor=True)
    ax1_1.set_ylim(bottom=40,top=350)
    ax1_1.set_xscale('log')
    ax1_1.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_1.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_1.set_xticks([1/9,1/3])
    #ax1_1.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_1.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res[systb['ae']][apptb['conv2d']].plot(kind='line', y="Slices", x="Parallelism", legend=False,
                                           ax=ax1_1, label="Scheduler Result", color=["g"],
                                           linestyle='-', marker='o', fontsize=fntsize,
                                           markersize=ms, linewidth=lw
                                           )
    print("plotting conv2d ae")
    print(res[systb['ae']][apptb['conv2d']])
    ax1_1.set_xlabel("", fontsize=fntsize);



    #conv2d_b2b
    conv2d_b2b_title = 'CONVB2B'
    ax1_2.set_title(conv2d_b2b_title)
    ax1_2.set_yscale('log')
    ax1_2.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    #ax1_2.yaxis.set_minor_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    ax1_2.minorticks_off()
    ax1_2.set_yticks([50,100,500])
    ax1_2.set_ylim(bottom=50, top=600)
    ax1_2.tick_params(axis='y', which='both', pad=20)
    ax1_2.set_xscale('log')
    ax1_2.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_2.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_2.set_xticks([1/9,1/3])
    #ax1_2.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_2.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res[systb['ae']][apptb['conv2d_b2b']].plot(kind='line', y="Slices", x="Parallelism", legend=False,
                                               ax=ax1_2, label="Scheduler Result", color=["g"],
                                               linestyle='-', marker='o', fontsize=fntsize,
                                               markersize=ms, linewidth=lw
                                               )
    print("plotting conv2d_b2b ae")
    print(res[systb['ae']][apptb['conv2d_b2b']])
    ax1_2.set_ylabel(y_label, fontsize=fntsize)
    ax1_2.set_xlabel(x_label, fontsize=fntsize);

    #sharpen
    sharpen_title = "SHARPEN"
    ax1_3.set_title(sharpen_title)
    ax1_3.set_yscale('log')
    #ax1_3.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    #ax1_3.yaxis.set_minor_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
    #ax1_3.majorticks_off()
    ax1_3.minorticks_off()
    ax1_3.tick_params(axis='both', which='major', labelsize=fntsize)
    ax1_3.set_yticks([])
    ax1_3.set_ylim(bottom=50, top=600)
    ax1_3.set_xscale('log')
    ax1_3.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
    ax1_3.set_xticks([1/9,1/3,1,2,4,8,16])
    #ax1_3.set_xticks([1/9,1/3])
    #ax1_3.set_xticklabels([r'$\frac{1}{3}$'])
    ax1_3.set_xticklabels([r'$\frac{1}{9}$',r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
    res[systb['ae']][apptb['sharpen']].plot(kind='line', y="Slices", x="Parallelism", legend=False,
                                            ax=ax1_3, label="Scheduler Result", color=["g"],
                                            linestyle='-', marker='o', fontsize=fntsize,
                                            markersize=ms, linewidth=lw
                                            )
    print("plotting sharpen ae")
    print(res[systb['ae']][apptb['sharpen']])
    ax1_3.set_xlabel(x_label, fontsize=fntsize);


    axes_slices = [x[0] for x in big_axes]
    axes_brams = [x[1] for x in big_axes]
    axes_dsps = [x[2] for x in big_axes]
    app_name = all_big_apps
    y_bottom_slices = [40] * len(axes_slices)
    y_top_slices = [3000] * len(axes_slices)
    y_ticks_slices = [[50,100,1000]] * len(axes_slices)
    y_ticks_brams = [2,8,32] * len(axes_slices)
    for index in range(len(app_name)):
        #ax1_4.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
        axes_slices[index].set_title(app_name[index].replace("_", " ") + " Slices")
        axes_slices[index].set_yscale('log')
        axes_slices[index].minorticks_off()
        axes_slices[index].set_ylim(bottom=y_bottom_slices[index], top=y_top_slices[index])
        axes_slices[index].set_yticks(y_ticks_slices[index])
        axes_slices[index].tick_params(axis='both', which='major', labelsize=fntsize)
        #axes_slices[index].set_ylim(bottom=50, top=600)
        axes_slices[index].set_xscale('log')
        axes_slices[index].xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
        axes_slices[index].set_xticks([1/3,1,2,4,8,16])
        axes_slices[index].set_xticklabels([r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
        res[systb['ae']][apptb[app_name[index]]].plot(kind='line', y="Slices", x="Parallelism", legend=False,
                                                   ax=axes_slices[index], label="Scheduler Result", color=["g"],
                                                   linestyle='-', marker='o', fontsize=fntsize,
                                                   markersize=ms, linewidth=lw
                                                   )
        if index == 0:
            axes_slices[index].set_ylabel(y_label, fontsize=fntsize)
        #axes_slices[index].set_xlabel(x_label, fontsize=fntsize);


        axes_brams[index].set_title(app_name[index].replace("_", " ") + " BRAMs")
        axes_brams[index].set_yscale('log')
        axes_brams[index].minorticks_off()
        axes_brams[index].yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
        axes_brams[index].tick_params(axis='both', which='major', labelsize=fntsize)
        axes_brams[index].set_yticks(y_ticks_brams)
        #axes_brams[index].set_ylim(bottom=50, top=600)
        axes_brams[index].set_xscale('log')
        axes_brams[index].xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
        axes_brams[index].set_xticks([1/3,1,2,4,8,16])
        axes_brams[index].set_xticklabels([r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
        res[systb['ae']][apptb[app_name[index]]].plot(kind='line', y="BRAMs", x="Parallelism", legend=False,
                                                   ax=axes_brams[index], label="Scheduler Result", color=["g"],
                                                   linestyle='-', marker='o', fontsize=fntsize,
                                                   markersize=ms, linewidth=lw
                                                   )

        #print(res[systb['ae']][apptb[app_name[index]]])
        if index == 0:
            axes_brams[index].set_ylabel(y_label, fontsize=fntsize)
        #axes_brams[index].set_xlabel(x_label, fontsize=fntsize);

        axes_dsps[index].set_title(app_name[index].replace("_", " ") + " DSPs")
        axes_dsps[index].set_yscale('log')
        axes_dsps[index].minorticks_off()
        axes_dsps[index].yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
        axes_dsps[index].tick_params(axis='both', which='major', labelsize=fntsize)
        axes_dsps[index].set_yticks(y_ticks_brams)
        #axes_dsps[index].set_ylim(bottom=50, top=600)
        axes_dsps[index].set_xscale('log')
        axes_dsps[index].xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
        axes_dsps[index].set_xticks([1/3,1,2,4,8,16])
        axes_dsps[index].set_xticklabels([r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
        res[systb['ae']][apptb[app_name[index]]].plot(kind='line', y="DSPs", x="Parallelism", legend=False,
                                                      ax=axes_dsps[index], label="Scheduler Result", color=["g"],
                                                      linestyle='-', marker='o', fontsize=fntsize,
                                                      markersize=ms, linewidth=lw
                                                      )

        print("plotting " + app_name[index])
        print(res[systb['ae']][apptb[app_name[index]]])
        if index == 0:
            axes_dsps[index].set_ylabel(y_label, fontsize=fntsize)
        #axes_dsps[index].set_xlabel(x_label, fontsize=fntsize);

    figs_dir = os.path.join(os.path.dirname(results_file), 'figs')
    plt.tight_layout()
    plt.savefig(os.path.join(figs_dir, 'ae_results_big.pdf'))

    fig_paper_ae, axes_paper_ae = plt.subplots(nrows=3, ncols=4, figsize=(24,13))
    plt.subplots_adjust(wspace=0.15, top=0.97)
    axes_slices = axes_paper_ae[0]
    axes_brams = axes_paper_ae[1]
    axes_dsps = axes_paper_ae[2]
    app_name = ["map"] + big_real_32_applications
    paper_titles = ["MAP", "CONV", "CONVB2B", "SHARPEN"]
    y_bottom_slices = [1] + ([100] * (len(axes_slices)-1))
    y_top_slices = [1000] + ([10000] * (len(axes_slices)-1))
    y_ticks_slices = [[100,1000,10000]] * len(axes_slices)
    y_ticks_brams = [[4,8,32]] * len(axes_slices)
    y_ticks_dsps = [[10,100,500]] * len(axes_slices)
    for index in range(len(app_name)):
        #ax1_4.yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
        #axes_slices[index].set_title(paper_titles[index])
        axes_slices[index].set_yscale('log')
        axes_slices[index].minorticks_off()
        axes_slices[index].set_ylim(bottom=y_bottom_slices[index], top=y_top_slices[index])
        axes_slices[index].tick_params(axis='both', which='major', labelsize=fntsize)
        #axes_slices[index].set_ylim(bottom=50, top=600)
        axes_slices[index].set_xscale('log')
        axes_slices[index].xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
        if index == 0:
            axes_slices[index].set_xticks([1,2, 5, 10, 20, 40, 200])
            axes_slices[index].set_xticklabels([r'$1$',r'$2$',r'$5$',r'$10$',r'$20$',r'$40$',r'$200$'])
        else:
            axes_slices[index].set_xticks([1/3,1,2,4,8,16])
            axes_slices[index].set_xticklabels([r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
        res[systb['ae']][apptb[app_name[index]]].plot(kind='line', y="Slices", x="Parallelism", legend=False,
                                                      ax=axes_slices[index], label="Scheduler Result", color=["g"],
                                                      linestyle='-', marker='o', fontsize=fntsize,
                                                      markersize=ms, linewidth=lw
                                                      )
        axes_slices[index].set_xlabel("", fontsize=fntsize)
        if index == 0:
            axes_slices[index].set_ylabel("Slices", fontsize=fntsize)
            axes_slices[index].set_yticks([1,10,1000])
        elif index == 1:
            axes_slices[index].set_yticks(y_ticks_slices[index])
        else:
            axes_slices[index].set_yticks([])
        #axes_slices[index].set_xlabel(x_label, fontsize=fntsize);


        #axes_brams[index].set_title(app_name[index].replace("_", " ") + " BRAMs")
        axes_brams[index].set_yscale('log')
        axes_brams[index].minorticks_off()
        axes_brams[index].yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
        axes_brams[index].tick_params(axis='both', which='major', labelsize=fntsize)
        axes_brams[index].set_ylim(bottom=4, top=32)
        axes_brams[index].set_xscale('log')
        axes_brams[index].xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
        if index == 0:
            axes_brams[index].set_xticks([1,2, 5, 10, 20, 40, 200])
            axes_brams[index].set_xticklabels([r'$1$',r'$2$',r'$5$',r'$10$',r'$20$',r'$40$',r'$200$'])
        else:
            axes_brams[index].set_xticks([1/3,1,2,4,8,16])
            axes_brams[index].set_xticklabels([r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
        res[systb['ae']][apptb[app_name[index]]].plot(kind='line', y="BRAMs", x="Parallelism", legend=False,
                                                      ax=axes_brams[index], label="Scheduler Result", color=["g"],
                                                      linestyle='-', marker='o', fontsize=fntsize,
                                                      markersize=ms, linewidth=lw
                                                      )
        axes_brams[index].set_xlabel("", fontsize=fntsize)
        #print(res[systb['ae']][apptb[app_name[index]]])
        if index == 0:
            axes_brams[index].set_ylabel("BRAMs", fontsize=fntsize)
            axes_brams[index].set_yticks(y_ticks_brams[index])
        else:
            axes_brams[index].set_yticks([])
        #axes_brams[index].set_xlabel(x_label, fontsize=fntsize);

        #axes_dsps[index].set_title(app_name[index].replace("_", " ") + " DSPs")
        axes_dsps[index].set_yscale('log')
        axes_dsps[index].minorticks_off()
        axes_dsps[index].yaxis.set_major_formatter(ticker.FuncFormatter(lambda y, _: '{:g}'.format(y)))
        axes_dsps[index].tick_params(axis='both', which='major', labelsize=fntsize)
        axes_dsps[index].set_ylim(bottom=10, top=650)
        axes_dsps[index].set_xscale('log')
        axes_dsps[index].xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:g}'.format(x)))
        if index == 0:
            axes_dsps[index].set_xticks([1,2, 5, 10, 20, 40, 200])
            axes_dsps[index].set_xticklabels([r'$1$',r'$2$',r'$5$',r'$10$',r'$20$',r'$40$',r'$200$'])
        else:
            axes_dsps[index].set_xticks([1/3,1,2,4,8,16])
            axes_dsps[index].set_xticklabels([r'$\frac{1}{3}$',r'$1$',r'$2$',r'$4$',r'$8$',r'$16$'])
        res[systb['ae']][apptb[app_name[index]]].plot(kind='line', y="DSPs", x="Parallelism", legend=False,
                                                      ax=axes_dsps[index], label="Scheduler Result", color=["g"],
                                                      linestyle='-', marker='o', fontsize=fntsize,
                                                      markersize=ms, linewidth=lw
                                                      )
        axes_dsps[index].set_xlabel(paper_titles[index])
        if index == 0:
            axes_dsps[index].set_ylabel("DSPs", fontsize=fntsize)
            axes_dsps[index].set_yticks(y_ticks_dsps[index])
        else:
            axes_dsps[index].set_yticks([])
    plt.savefig(os.path.join(figs_dir, 'ae_results.pdf'))

    sp_maxes = []
    sp_mins = []
    hth_maxes = []
    hth_mins = []
    joined_sp_res_list = []
    joined_hth_res_list = []
    joined_sp_ratios_list = []
    joined_hth_slices_ratios_list = []
    joined_hth_brams_ratios_list = []
    for app in apptb:
        #print("App " + str(app))
        (joined_sp_res, joined_hth_res, joined_sp_ratios, joined_hth_slices_ratios, joined_hth_brams_ratios) = \
            comp_ae_and_others(res[systb['ae']][apptb[app]], res[systb['h2h']][apptb[app]], res[systb['sp']][apptb[app]])
        joined_sp_res_list.append(joined_sp_res)
        joined_hth_res_list.append(joined_hth_res)
        joined_sp_ratios_list.append(joined_sp_ratios)
        joined_hth_slices_ratios_list.append(joined_hth_slices_ratios)
        joined_hth_brams_ratios_list.append(joined_hth_brams_ratios)
        #print("Max SP" + str(joined_sp_ratios.iloc[:,1].max()))
        #print("Min SP" + str(joined_sp_ratios.iloc[:,1].min()))
        #print("Max HTH" + str(joined_hth_ratios.iloc[:,1].max()))
        #print("Min HTH" + str(joined_hth_ratios.iloc[:,1].min()))
        sp_maxes.append(joined_sp_ratios.iloc[:,1].max())
        sp_mins.append(joined_sp_ratios.iloc[:,1].min())
        hth_maxes.append(joined_hth_slices_ratios.iloc[:,1].max())
        hth_mins.append(joined_hth_brams_ratios.iloc[:,1].min())
    #print("AE_SP_Slices_Ratio max: " + str(max(sp_maxes)))
    #print("AE_SP_Slices_Ratio min: " + str(min(sp_mins)))
    #print("hth_maxes: " + str(hth_maxes))
    #print("hth_mins: " + str(hth_mins))





    fig, ((ax2_0_0, ax2_0_1, ax2_0_2, ax2_0_3), (ax2_1_0, ax2_1_1, ax2_1_2, ax2_1_3), (ax2_2_0, ax2_2_1, ax2_2_2, ax2_2_3)) = plt.subplots(nrows=3, ncols=4)
    plt.subplots_adjust(wspace=0, top=0.97)
    plt.rc('text', usetex=True)
    plt.rcParams.update({'font.size': fntsize})
    fig.set_figwidth(18)
    fig.set_figheight(10)

    def plot_bar_comp(axis_slices, axis_brams, axis_dsps, title, appname, leftmost=False, rightmost=False):
        joined_sp_ratios_list[apptb[appname]].fillna(0)
        if leftmost:
            axis_slices.set_yticks([0,1,2,4,6,8,10])
            axis_brams.set_yticks([0,1,2,3,4])
            axis_dsps.set_yticks([0.,0.2,0.4,0.6,0.8,1.0])
        else:
            axis_slices.set_yticks([])
            axis_brams.set_yticks([])
            axis_dsps.set_yticks([])
        joined_sp_ratios_list[apptb[appname]].plot(kind='bar', y="AE_SP_Slices_Ratio", x="Parallelism", rot=0,
                                              ax=axis_slices, legend=False, color=["g"], width=0.8,
                                              fontsize=fntsize)
        joined_sp_ratios_list[apptb[appname]].plot(kind='bar', y="AE_SP_BRAMs_Ratio", x="Parallelism", rot=0,
                                                   ax=axis_brams, legend=False, color=["g"], width=0.8,
                                                   fontsize=fntsize)
        joined_sp_ratios_list[apptb[appname]].plot(kind='bar', y="AE_SP_DSPs_Ratio", x="Parallelism", rot=0,
                                                   ax=axis_dsps, legend=False, color=["g"], width=0.8,
                                                   fontsize=fntsize)
        axis_slices.set_xticklabels([r'$1$',r'$2$',r'$4$',r'$8$'])
        axis_brams.set_xticklabels([r'$1$',r'$2$',r'$4$',r'$8$'])
        axis_dsps.set_xticklabels([r'$1$',r'$2$',r'$4$',r'$8$'])
        axis_slices.set_ylim(bottom=0,top=10)
        axis_brams.set_ylim(bottom=0,top=4.5)
        axis_dsps.set_ylim(bottom=0,top=1)
        print("plotting " + str(appname) + " Aetherling vs Spatial")
        print(joined_sp_ratios_list[apptb[appname]])

        axis_slices.spines['right'].set_visible(rightmost == False)
        axis_brams.spines['right'].set_visible(rightmost == False)
        axis_dsps.spines['right'].set_visible(rightmost == False)

        axis_slices.spines['right'].set_linewidth(3)
        axis_brams.spines['right'].set_linewidth(3)
        axis_dsps.spines['right'].set_linewidth(3)

        axis_slices.spines['left'].set_visible(leftmost == True)
        axis_brams.spines['left'].set_visible(leftmost == True)
        axis_dsps.spines['left'].set_visible(leftmost == True)

        axis_slices.spines['top'].set_visible(False)
        axis_brams.spines['top'].set_visible(False)
        axis_dsps.spines['top'].set_visible(False)

        if leftmost == True:
            axis_slices.set_ylabel("Slices Ratio", fontsize=fntsize)
            axis_brams.set_ylabel("BRAMs Ratio", fontsize=fntsize)
            axis_dsps.set_ylabel("DSPs Ratio", fontsize=fntsize)
        axis_slices.set_xlabel("", fontsize=fntsize);
        axis_brams.set_xlabel("", fontsize=fntsize);
        axis_dsps.set_xlabel(title.replace("_", " "), fontsize=fntsize);

    plot_bar_comp(ax2_0_0, ax2_1_0, ax2_2_0, map_title, 'map', leftmost=True)
    plot_bar_comp(ax2_0_1, ax2_1_1, ax2_2_1, 'CONV2D', "big_real_32_conv2d")
    plot_bar_comp(ax2_0_2, ax2_1_2, ax2_2_2, 'CONV2D B2B', "big_real_32_conv2d_b2b")
    plot_bar_comp(ax2_0_3, ax2_1_3, ax2_2_3, 'SHARPEN', "big_real_32_sharpen", rightmost=True)

    plt.savefig(os.path.join(figs_dir, 'ae_versus_sp.pdf'))

    hth_p1_slices_values = []
    hth_p1_brams_values = []
    hth_p1_titles = ["map", "big_32_conv2d", "big_32_conv2d_b2b", "big_32_sharpen"]
    for title in hth_p1_titles:
        i = apptb[title]
        hth_p1_slices_values.append(joined_hth_slices_ratios_list[i].loc[joined_hth_slices_ratios_list[i].loc[:, 'Parallelism'] == 1, :].iloc[0,1])
        hth_p1_brams_values.append(joined_hth_brams_ratios_list[i].loc[joined_hth_brams_ratios_list[i].loc[:, 'Parallelism'] == 1, :].iloc[0,1])

    hth_p1_slices_df = pd.DataFrame.from_dict({
        'apps': [x.replace("_", " ") for x in hth_p1_titles],
        'values': hth_p1_slices_values
    })
    print("Halide-HLS Single Chart Slices")
    print(hth_p1_slices_df)

    hth_p1_brams_df = pd.DataFrame.from_dict({
        'apps': [x.replace("_", " ") for x in hth_p1_titles],
        'values': hth_p1_brams_values
    })
    print("Halide-HLS Single Chart BRAMs")
    print(hth_p1_brams_df)

    fig, ax3 = plt.subplots()
    plt.subplots_adjust(top=0.99, bottom=0.19)
    fig.set_figwidth(11)
    fig.set_figheight(4)
    plt.rc('text', usetex=True)
    plt.rcParams.update({'font.size': fntsize})
    #ax3.set_title("Halide-HLS/Aetherling Ratio of Area (Slices)")
    ax3.spines['right'].set_visible(False)
    ax3.spines['top'].set_visible(False)
    hth_p1_slices_df.plot(kind='bar', y='values', x='apps', rot=10,
                   ax=ax3, legend=False, color=["g"],
                   fontsize=fntsize)
    #ax3_1.spines['right'].set_visible(False)
    #ax3_1.spines['top'].set_visible(False)
    #hth_p1_brams_df.plot(kind='bar', y='values', x='apps', rot=10,
    #                          ax=ax3_1, legend=False, color=["g"],
    #                          fontsize=fntsize)
    plt.savefig(os.path.join(figs_dir, 'ae_versus_hth.pdf'))

    apps_to_print_sp = ['map', "big_real_32_conv2d", "big_real_32_conv2d_b2b", 'big_real_32_sharpen']
    apps_to_print_hth = ['map', "big_32_conv2d", "big_32_conv2d_b2b", 'big_32_sharpen']
    for appname in apps_to_print_sp:
        print("printing all comp res for " + str(appname))
        print(joined_sp_res_list[apptb[appname]])
    for appname in apps_to_print_hth:
        print("printing all comp res for " + str(appname))
        print(joined_hth_res_list[apptb[appname]])
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
    ae_pd = ae_pd.rename(columns={"Slices": "AE_Slices", "Parallelism": "AE_Parallelism", "BRAMs": "AE_BRAMs",
                                  "DSPs": "AE_DSPs"}).loc[:,["AE_Slices", "AE_Parallelism", "AE_BRAMs", "AE_DSPs"]]
    hth_pd = hth_pd.rename(columns={"Slices": "HTH_Slices", "Parallelism": "HTH_Parallelism","BRAMs": "HTH_BRAMs",
                                  "DSPs": "HTH_DSPs"}).loc[:,["HTH_Slices", "HTH_Parallelism", "HTH_BRAMs", "HTH_DSPs"]]
    sp_pd = sp_pd.rename(columns={"Slices": "SP_Slices", "Parallelism": "SP_Parallelism", "BRAMs": "SP_BRAMs",
                                  "DSPs": "SP_DSPs"}).loc[:, ["SP_Slices", "SP_Parallelism", "SP_BRAMs", "SP_DSPs"]]
    joined_sp = ae_pd.merge(sp_pd, left_on='AE_Parallelism', right_on='SP_Parallelism', how="inner")
    joined_res_sp = joined_sp
    joined_res_sp.loc[:,'Parallelism'] = joined_res_sp.loc[:,'AE_Parallelism']
    joined_hth = ae_pd.merge(hth_pd, left_on='AE_Parallelism', right_on='HTH_Parallelism', how="inner")
    joined_res_hth = joined_hth
    joined_res_hth.loc[:,'Parallelism'] = joined_res_hth.loc[:,'AE_Parallelism']
    joined_sp.loc[:,'Parallelism'] = joined_sp.loc[:,'SP_Parallelism']
    joined_hth.loc[:,'Parallelism'] = joined_hth.loc[:,'HTH_Parallelism']
    joined_sp.loc[:,'AE_SP_Slices_Ratio'] = joined_sp.loc[:,'SP_Slices'] / joined_sp.loc[:, 'AE_Slices']
    joined_sp.loc[:,'AE_SP_BRAMs_Ratio'] = joined_sp.loc[:,'SP_BRAMs'] / joined_sp.loc[:, 'AE_BRAMs']
    joined_sp.loc[:,'AE_SP_DSPs_Ratio'] = joined_sp.loc[:,'SP_DSPs'] / joined_sp.loc[:, 'AE_DSPs']
    joined_hth.loc[:,'AE_HTH_Slices_Ratio'] = joined_hth.loc[:,'HTH_Slices'] / joined_hth.loc[:, 'AE_Slices']
    joined_hth.loc[:,'AE_HTH_BRAMs_Ratio'] = joined_hth.loc[:,'HTH_BRAMs'] / joined_hth.loc[:, 'AE_BRAMs']
    joined_sp_ratios = joined_sp.loc[:,['Parallelism', 'AE_SP_Slices_Ratio', 'AE_SP_BRAMs_Ratio', 'AE_SP_DSPs_Ratio']]
    joined_hth_slices_ratios = joined_hth.loc[:,['Parallelism', 'AE_HTH_Slices_Ratio']]
    joined_hth_brams_ratios = joined_hth.loc[:,['Parallelism', 'AE_HTH_BRAMs_Ratio']]
    joined_res_sp = joined_res_sp.loc[:, ['Parallelism', 'AE_Slices', 'AE_BRAMs', 'AE_DSPs',
                                    'SP_Slices', 'SP_BRAMs', 'SP_DSPs']]#.rename(
    joined_res_hth = joined_res_hth.loc[:, ['Parallelism', 'AE_Slices', 'AE_BRAMs', 'AE_DSPs',
                                          'HTH_Slices', 'HTH_BRAMs']]  # .rename(
    #columns={"AE_Slices":"Aetherling","SP_Slices":"Spatial","HTH_Slices":"Halide-HLS"}
    #)
    #print(joined_sp_ratios)
    #print(joined_hth_ratios)
    return (joined_res_sp, joined_res_hth, joined_sp_ratios, joined_hth_slices_ratios, joined_hth_brams_ratios)

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
    # https://forums.xilinx.com/t5/Design-Entry/RAMB36-vs-RAMB18/td-p/150442 - this is old link, ignore, here for history
    # p. 11 of - https://www.xilinx.com/support/documentation/user_guides/ug473_7Series_Memory_Resources.pdf 
    results_pd['BRAMs'] = results_pd['BRAMTiles']
    results_pd['DSPs'] = results_pd['DSP48Blocks']
    results_pd['Flip Flops'] = results_pd['FFs']
    results_pd['Shift Registers'] = results_pd['SRLs']
    results_pd['Slices'] = results_pd['Slices']
    results_pd['Parallelism'] = results_pd['Parallelism']
    results_pd['MHz'] = results_pd['Slack(VIOLATED)'].apply(fix_clock)
    return results_pd.loc[:, ['Parallelism', 'LUTs', 'BRAMs', 'DSPs', 'Flip Flops', 'Shift Registers', 'Slices', 'MHz']]

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
