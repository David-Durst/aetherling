import pandas as pd
from math import isnan, nan
from fractions import Fraction as frac

def get_latex_from_results_str(results_file):
    results = pd.read_csv(results_file)
    results['Clock Rate'] = nan
    results_tex_str = ""
    systems = ["aetherling_copies", "halide_to_hardware", "spatial"]
    applications = ["map", "pyramid", "sharpen", "camera"]
    application_lengths = [200, 200, 200, 200]
    application_parallelisms = [[0.125, 0.25, 0.5, 1,2,4,5,10,20,200], [0.125, 0.25, 0.5, 1,2,4,8,16,32,64,128,256], [0.125, 0.25, 0.5, 1,2,4,8,16,32,64,128,256], [0.125, 0.25, 0.5, 1,2,4,8,16,32,64,128,256]]
    per_system_per_application_results = []
    for i, system in enumerate(systems):
        per_system_results = []
        for j, app in enumerate(applications):
            start_per_app_per_system = results[(results.System == system) & (results.Application == app)]
            paper_parallelism = fix_parallelism(start_per_app_per_system, application_lengths[j])
            filled_in = add_missing_parallelisms(paper_parallelism, system, app, application_parallelisms[j])
            sorted_by_parallelism = filled_in.sort_values("Parallelism")
            results_only_selected_columns = get_output_columns(sorted_by_parallelism)
            per_system_results.append(results_only_selected_columns)
        per_system_per_application_results.append(per_system_results)
#    per_system_results = [results[results.System == system] for system in systems]
#    per_system_per_application = \
#        [[per_system_result[per_system_result.Application == app]
#          for app in applications]
#         for per_system_result in per_system_results]


    # get all Aetherling results into latex tables
    #aetherling_per_app = per_system_per_application_results[0]
    for i, system_per_app in enumerate(per_system_per_application_results):
        for j, app_pd in enumerate(system_per_app):
            results_tex_str += "System {}, App {}\n".format(systems[i], applications[j])
            results_tex_str += app_pd.to_latex(index=False, escape=False)
    for app_idx in range(len(applications)):
        results_tex_str += "Comparison for App {}\n".format(applications[j])
        results_tex_str += merge_columns(
            per_system_per_application_results[0][app_idx],
            per_system_per_application_results[1][app_idx],
            per_system_per_application_results[2][app_idx],
        ).to_latex(index=False, escape=False)
    return results_tex_str


def add_missing_parallelisms(results_pd, system, application, parallelisms_to_add):
    for p in parallelisms_to_add:
        if p not in results_pd.Parallelism.values:
            results_pd = results_pd.append({"System": system, "Application":application, "Parallelism": p}, ignore_index=True)
    return results_pd

def fix_parallelism(results_pd, length):
    results_pd['Parallelism'] = results_pd['Parallelism'].apply(lambda x: int(length / x))
    return results_pd

def get_output_columns(results_pd):
    results_pd['LUTs'] = results_pd['TotalLUTs'].apply(int_if_not_nan)
    results_pd['BRAMs'] = results_pd['RAMB36'] + results_pd['RAMB18']
    results_pd['BRAMs'] = results_pd['BRAMs'].apply(int_if_not_nan)
    results_pd['Slices'] = results_pd['Slices'].apply(int_if_not_nan)
    results_pd['Parallelism'] = results_pd['Parallelism'].apply(int_if_not_nan)
    results_pd['Clock Rate'] = results_pd['Clock Rate'].apply(int_if_not_nan)
    return results_pd[['Parallelism', 'LUTs', 'BRAMs', 'Slices', 'Clock Rate']]

def percent_vs_aetherling(aetherling_results, other_results, column_name):
    others = pd.to_numeric(other_results[column_name], errors='coerce')
    ae = pd.to_numeric(aetherling_results[column_name], errors='coerce')
    return ((others - ae) / ae).apply(int_if_not_nan)

def merge_columns(aetherling_results, halide_results, spatial_results):
    aetherling_results['Aetherling\nLUTs'] = aetherling_results['LUTs']
    aetherling_results['Aetherling\nBRAMs'] = aetherling_results['BRAMs']
    aetherling_results['Aetherling\nSlices'] = aetherling_results['Slices']
    aetherling_results['HLUTs'] = percent_vs_aetherling(aetherling_results, halide_results, 'LUTs')
    aetherling_results['HBRAMs'] = percent_vs_aetherling(aetherling_results, halide_results, 'BRAMs')
    aetherling_results['HSlices'] = percent_vs_aetherling(aetherling_results, halide_results, 'Slices')
    aetherling_results['SLUTs'] = percent_vs_aetherling(aetherling_results, spatial_results, 'LUTs')
    aetherling_results['SBRAMs'] = percent_vs_aetherling(aetherling_results, spatial_results, 'BRAMs')
    aetherling_results['SSlices'] = percent_vs_aetherling(aetherling_results, spatial_results, 'Slices')
    return aetherling_results[['Parallelism',
                               'HLUTs', 'HBRAMs', 'HSlices',
                               'SLUTs', 'SBRAMs', 'SSlices',
                               ]]


def int_if_not_nan(x):
    if isnan(x):
        return "\\red{X}"
    elif x == int(x):
        return str(int(x))
    else:
        fr = frac(x)
        #return "$\\frac{" + str(fr.numerator) + "}{" + str(fr.denominator) + "}$"
        return str(fr.numerator) + "/" + str(fr.denominator)
