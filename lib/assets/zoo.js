import { Base } from "./base.js";

const ZooModel = (current_namespace, opts) => {
  return {
    name: "ZooModel",

    components: {
      BaseInput: Base.BaseInput,
      BaseSelect: Base.BaseSelect,
    },

    props: {
      fields: {
        type: Object,
        default: {},
      },
      tasks: {
        type: Array,
        default: [],
      }
    },

    data() {
      return {
        showHelpBox: true,
        taskOptions: this.tasks.map((task) => ({
          value: task.id,
          label: task.label,
        })),
        backendOptions: [
          { value: "opencv", label: "OpenCV" },
          { value: "cuda", label: "CUDA" },
          { value: "timvx", label: "TIMVX" },
        ],
        targetOptions: [
          { value: "cpu", label: "CPU" },
          { value: "cuda", label: "CUDA" },
          { value: "cuda_f16", label: "CUDA FP16" },
        ],
      };
    },

    computed: {
      selectedTask() {
        return this.tasks.find((task) => task.id === this.fields.task_id);
      },

      selectedVariant() {
        return this.selectedTask.variants.find(
          (variant) => variant.id === this.fields.variant_id
        );
      },

      variantOptions() {
        return this.selectedTask.variants.map((variant) => ({
          value: variant.id,
          label: variant.label,
        }));
      },
    },

    methods: {
      toggleHelpBox(_) {
        this.showHelpBox = !this.showHelpBox;
      },
    },

    template: `
    <div class="header">
      <BaseSelect
        name="task_id"
        label="Task"
        :value="fields.task_id"
        selectClass="input"
        :inline
        :options="taskOptions"
      />
      <div class="variant-container">
        <BaseSelect
          name="variant_id"
          label="Using"
          :value="fields.variant_id"
          selectClass="input"
          :inline
          :options="variantOptions"
        />
      </div>
      <div class="grow"></div>
      <button type="button" @click="toggleHelpBox" class="icon-button">
        <i class="ri ri-questionnaire-line" aria-hidden="true"></i>
      </button>
    </div>
    <div class="help-section" v-if="!showHelpBox">
      <div>
        <p>This smart cell showcases models available in the <a href="https://github.com/opencv/opencv_zoo" target="_blank">opencv_zoo</a> repository.</p>
      </div>
    </div>
    <div v-for="param in selectedVariant.params">
      <label class="param-collection">{{ param.name }}</label>
      <div class="row">
        <div v-for="pparam in param.params">
          <div v-if="pparam.is_option">
            <BaseSelect
              :name="pparam.field"
              :label="pparam.label"
              :value="fields[pparam.field]"
              selectClass="input"
              :options="pparam.options"
            />
          </div>
          <div v-else>
            <BaseInput
              :name="pparam.field"
              :label="pparam.label"
              :labelTooltip="pparam.tooltip"
              :type="pparam.type"
              :value="fields[pparam.field]"
              inputClass="input input--xs"
            />
          </div>
        </div>
      </div>
    </div>
    <label class="param-collection">Inference</label>
    <div class="row">
      <div>
        <BaseSelect
          name="backend"
          label="Backend"
          :value="fields.backend"
          selectClass="input"
          :options="backendOptions"
        />
      </div>
      <div>
        <BaseSelect
          name="target"
          label="Target"
          :value="fields.target"
          selectClass="input"
          :options="targetOptions"
        />
      </div>
    </div>
    `,
  }
};

const ZooSmartCellConfig = (ctx, info) => {
  const extra_components = {
    ZooModel: ZooModel("", {}),
  };
  var config = Base.SmartCellConfig(ctx, info, extra_components, {tasks: info.tasks});
  config.template = `
  <div class="app">
    <form @change="handleFieldChange">
      <div class="container">
        <ZooModel v-bind:fields="fields" v-bind:tasks="tasks" />
      </div>
    </form>
  </div>
  `;

  return config;
}

export const Zoo = {
    SmartCell: {
        "evision.zoo": ZooSmartCellConfig,
    }
};
