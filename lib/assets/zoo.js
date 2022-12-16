import { Base } from "./base.js";
import "https://cdn.jsdelivr.net/npm/marked/marked.min.js";
import "https://cdn.jsdelivr.net/npm/dompurify/dist/purify.min.js";

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
      },
      backend_options: {
        type: Array,
        default: [],
      },
      target_options: {
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
        }))
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

      selectedVariantDocs() {
        return DOMPurify.sanitize(marked.parse(this.selectedVariant.docs, { sanitize: true }));
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
      <div v-html="selectedVariantDocs"></div>
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
          :options="backend_options"
        />
      </div>
      <div>
        <BaseSelect
          name="target"
          label="Target"
          :value="fields.target"
          selectClass="input"
          :options="target_options"
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
  var config = Base.SmartCellConfig(ctx, info, extra_components, {
    tasks: info.tasks, 
    backend_options: info.backend_options, 
    target_options: info.target_options}
  );
  config.template = `
  <div class="app">
    <form @change="handleFieldChange">
      <div class="container">
        <ZooModel v-bind:fields="fields" v-bind:tasks="tasks" v-bind:backend_options="backend_options" v-bind:target_options="target_options" />
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
